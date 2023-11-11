{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Duster.CartPole where

import Control.Concurrent.PooledIO.Independent (run)
import Data.Data (Proxy (Proxy))
import Data.Default (def)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Data.VectorSpace (AdditiveGroup, InnerSpace, VectorSpace)
import Duster.Learning (AdamBufs, AdamConfig, AdamParam)
import qualified Duster.Learning as L
import Duster.LinearFit (relFileNum)
import Duster.Log (Logger, logString)
import Duster.Loss (listMean)
import Duster.ODEInt (Grad (Grad), odeIntRK4, termt)
import Duster.Ops (tuple4ToVec, vecToScalar)
import Duster.Orphans ()
import Duster.PRNG (initLinearWeight)
import GHC.Generics (Generic)
import Generics.Deriving (GEq, geq)
import Generics.Deriving.Show (GShow, gshow, gshowList, gshowsPrec)
import qualified Graphics.Rendering.Chart as C
import Graphics.Rendering.Chart.Backend.Cairo
  ( FileFormat (PNG),
    FileOptions (FileOptions),
    toFile,
  )
import Graphics.Rendering.Chart.Easy ((.=))
import qualified Graphics.Rendering.Chart.Easy as CE
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Numeric.Backprop
  ( Backprop,
    Reifies,
    W,
    collectVar,
    constVar,
    evalBP,
    gradBP,
    joinBV,
    splitBV,
    pattern T2,
  )
import Numeric.LinearAlgebra.Static.Backprop (BVar, L, R, (#>))
import Path (Abs, Dir, File, Path, toFilePath, (</>))
import System.Random (mkStdGen, random, split)

---- State of the CartPole system ---------------------------------------------

type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

-- | CartPole state.
data State' f = State
  { _x :: HKD f Double,
    _x_dot :: HKD f Double,
    _theta :: HKD f Double,
    _theta_dot :: HKD f Double,
    _effort :: HKD f Double
  }
  deriving (Generic)

type State = State' Identity

instance GEq State

instance Eq State where
  (==) = geq

instance GShow State

instance Show State where
  showsPrec = gshowsPrec
  show = gshow
  showList = gshowList

instance Backprop State

instance AdditiveGroup State

instance VectorSpace State

instance InnerSpace State

---- Equations of Motion ------------------------------------------------------

data CartPoleConfig = CartPoleConfig
  { _cp_length :: Double,
    _cp_cart_mass :: Double,
    _cp_pend_mass :: Double,
    _cp_g_accel :: Double,
    _cp_force_mul :: Double
  }
  deriving (Show, Generic)

makeLenses ''CartPoleConfig

defaultCartPoleConfig :: CartPoleConfig
defaultCartPoleConfig =
  let pend_mass = 1 -- kg
      cart_mass = 2 -- kg
      pend_len = 1 -- m
      g_accel = 9.81 -- m/s/s
      force_mul = 120 -- N (max force)
   in CartPoleConfig pend_len cart_mass pend_mass g_accel force_mul

eom ::
  forall s.
  (Reifies s W) =>
  CartPoleConfig ->
  BVar s Double ->
  BVar s State ->
  Grad (BVar s State)
eom config force state =
  let State _x xd t td _e = splitBV state

      l = constVar $ config ^. cp_length
      m1 = constVar $ config ^. cp_cart_mass
      m2 = constVar $ config ^. cp_pend_mass
      g = constVar $ config ^. cp_g_accel
      f = force * constVar (config ^. cp_force_mul)

      deffort = force ^ (2 :: Int)

      s = sin t
      c = cos t
      den = m2 * c ^ (2 :: Int) - (m1 + m2)

      xdd = (-m2 * g * s * c - (f + m2 * l * td ^ (2 :: Int) * s)) / den
      tdd = ((m1 + m2) * g * s + c * (f + m1 * l * td ^ (2 :: Int) * s)) / (l * den)
   in Grad . joinBV $ State xd xdd td tdd deffort

---- Controller ---------------------------------------------------------------

data Net' f = Net
  { _weight1 :: HKD f (L 24 4),
    _bias1 :: HKD f (R 24),
    _weight2 :: HKD f (L 48 24),
    _bias2 :: HKD f (R 48),
    _weight3 :: HKD f (L 1 48),
    _bias3 :: HKD f (R 1)
  }
  deriving (Generic)

type Net = Net' Identity

instance GShow Net

instance Show Net where
  showsPrec = gshowsPrec
  show = gshow
  showList = gshowList

instance Backprop Net

instance AdditiveGroup Net

instance VectorSpace Net

instance AdamParam Net

-- | Initialize the network.
--
-- Weights are initialized using random values drawn from a uniform
-- distribution, with ranges determined by the number of input features.
-- This follows the PyTorch initialization approach.
initNet :: Net
initNet =
  let seed = 1
      (g1, g1') = split (mkStdGen seed)
      (g2, g2') = split g1'
      (g3, _) = split g2'
      w1 = initLinearWeight g1
      b1 = 0
      w2 = initLinearWeight g2
      b2 = 0
      w3 = initLinearWeight g3
      b3 = 0
   in Net w1 b1 w2 b2 w3 b3

logistic :: (Floating a) => a -> a
logistic x = 1 / (1 + exp (-x))

controller ::
  forall s.
  (Reifies s W) =>
  BVar s Net ->
  BVar s State ->
  BVar s Double
controller net state =
  let State x _xd t td _e = splitBV state
      Net w1 b1 w2 b2 w3 b3 = splitBV net
      input = tuple4ToVec (x, sin t, cos t, td / pi)
      x1 = tanh $ w1 #> input + b1
      x2 = tanh $ w2 #> x1 + b2
      x3 = tanh $ w3 #> x2 + b3
   in vecToScalar x3

---- System Dynamics: EOM + Controller ----------------------------------------

sysDyn ::
  forall s.
  (Reifies s W) =>
  CartPoleConfig ->
  BVar s Net ->
  BVar s State ->
  Grad (BVar s State)
sysDyn config net cur_state =
  let force = controller net cur_state
   in eom config force cur_state

---- Integrate System Dynamics ------------------------------------------------

integrate ::
  forall s.
  (Reifies s W) =>
  -- | Time step (seconds).
  Double ->
  -- | Final time (seconds).
  Double ->
  -- | System dynamics.
  (BVar s State -> Grad (BVar s State)) ->
  -- | Initial state.
  BVar s State ->
  -- | Evolution of system states over time.
  BVar s [(Double, State)]
integrate t_step t_final grad_fn init_state =
  let stop_fn :: BVar s Double -> BVar s State -> Bool
      stop_fn = termt (constVar t_final)

      init_ts :: (BVar s Double, BVar s State)
      init_ts = (constVar 0, init_state)

      ts :: [(BVar s Double, BVar s State)]
      ts = odeIntRK4 (constVar t_step) (const grad_fn) stop_fn init_ts
   in collectVar (uncurry T2 <$> ts)

{-

---- Loss ---------------------------------------------------------------------

lossBalancing ::
  forall s.
  (Reifies s W) =>
  -- | System trajectory.
  BVar s [(Double, State)] ->
  -- | Overall computed loss.
  BVar s Double
lossBalancing trajectory =
  let angle_loss_fn :: BVar s (Double, State) -> BVar s Double
      angle_loss_fn ts =
        let (_, _, angle, _, _) = vecToTuple5 (bvarSnd ts ^^. unState)
            angle_err = angle - pi
         in angle_err ^ (2 :: Int)

      loss_angle =
        smootherStepWeightedTrajectoryLoss (-1) 0 angle_loss_fn trajectory
   in loss_angle

lossSwingup ::
  forall s.
  (Reifies s W) =>
  -- | System trajectory.
  BVar s [(Double, State)] ->
  -- | Overall computed loss.
  BVar s Double
lossSwingup trajectory =
  let angle_loss_fn :: BVar s (Double, State) -> BVar s Double
      angle_loss_fn ts =
        let (_, _, angle, _, _) = vecToTuple5 (bvarSnd ts ^^. unState)
            angle_err = angle - pi
         in angle_err ^ (2 :: Int)

      loss_angle =
        smootherStepWeightedTrajectoryLoss 1 1.5 angle_loss_fn trajectory
   in loss_angle

loss ::
  forall s.
  (Reifies s W) =>
  -- | System trajectory.
  BVar s [(Double, State)] ->
  -- | Overall computed loss.
  BVar s Double
loss trajectory =
  let weight_x = 0.6
      weight_angle = 1.0
      weight_effort = 0.05

      x_loss_fn :: BVar s (Double, State) -> BVar s Double
      x_loss_fn ts =
        let (x, _, _, _, _) = vecToTuple5 (bvarSnd ts ^^. unState)
         in x ^ (2 :: Int)

      angle_loss_fn :: BVar s (Double, State) -> BVar s Double
      angle_loss_fn ts =
        let (_, _, angle, _, _) = vecToTuple5 (bvarSnd ts ^^. unState)
            angle_err = angle - pi
         in angle_err ^ (2 :: Int)

      loss_effort :: BVar s Double
      loss_effort =
        let (_, _, _, _, e) =
              vecToTuple5
                (bvarSnd (last (sequenceVar trajectory)) ^^. unState)
         in e ^ (2 :: Int)

      loss_x =
        smootherStepWeightedTrajectoryLoss 0.8 1.0 x_loss_fn trajectory
      loss_angle =
        smootherStepWeightedTrajectoryLoss 0.5 0.9 angle_loss_fn trajectory
   in weight_x * loss_x
        + weight_angle * loss_angle
        + weight_effort * loss_effort

{-
loss ::
  forall s.
  (Reifies s W) =>
  -- | System trajectory.
  BVar s [(Double, State)] ->
  -- | Overall computed loss.
  BVar s Double
loss traj =
  let states :: [BVar s State]
      states = bvarSnd <$> sequenceVar traj

      -- TTDS are the values of (x, theta, theta_dot, effort)
      ttds :: [(BVar s Double, BVar s Double, BVar s Double, BVar s Double)]
      ttds = f <$> states
        where
          f :: BVar s State -> (BVar s Double, BVar s Double, BVar s Double, BVar s Double)
          f s = let (x, _, t, td, e) = vecToTuple5 (s ^^. unState) in (x, t, td, e)

      -- theta_f = (\(_, theta, _) -> theta) $ last ttds
      -- loss_theta_f = 0.5 * cos theta_f

      -- theta_dot_f = (\(_, _, theta_dot) -> theta_dot) $ last ttds
      -- loss_theta_dot_f = theta_dot_f ^ (2 :: Int)

      xs = (\(x, _, _, _) -> x) <$> ttds
      theta = (\(_, t, _, _) -> t) <$> ttds
      theta_dot = (\(_, _, td, _) -> td) <$> ttds
      effort = (\(_, _, _, e) -> e) <$> ttds

      loss_x = listMeanSq xs
      loss_x_f = last xs ^ (2 :: Int)
      loss_theta = listMeanSq ((\t -> t - pi) <$> theta)
      loss_theta_f = (last theta - pi) ^ (2 :: Int)
      loss_theta_dot = listMeanSq theta_dot
      loss_theta_dot_f = last theta_dot ^ (2 :: Int)
      loss_effort_f = last effort ^ (2 :: Int)
   -- in loss_theta_f + 0.3 * loss_theta + 0.1 * loss_x + 0.1 * loss_x_f + 0.0 * loss_theta_dot + 0.0 * loss_theta_dot_f + 0.1 * loss_effort_f
   in 4 * loss_theta_f + loss_theta + 0.2 * loss_x + 0.1 * loss_effort_f -- + 0.5 * loss_x + 0.1 * loss_effort_f
-}

-- 'snd' lifted to operate over a tuple inside a 'BVar'.
bvarSnd ::
  (Backprop a, Backprop b, Reifies s W) =>
  BVar s (a, b) ->
  BVar s b
bvarSnd (T2 _ y) = y
bvarSnd _ = error "This case should not occur"

---- Training -----------------------------------------------------------------

trainStepBatch ::
  -- | Configuration of the cartpole system.
  CartPoleConfig ->
  -- | Starting states for the batch.
  [State] ->
  -- | Configuration of the Adam learning update.
  AdamConfig Double ->
  -- | Initial Adam learning buffers.
  AdamBufs Net ->
  -- | Initial controller network.
  Net ->
  -- | Updated controller network and updated Adam learning buffers.
  (Net, AdamBufs Net)
trainStepBatch config batch adam_config init_adam_bufs init_net =
  let t_step, t_final :: Double
      -- Time step for RK4 integration.
      t_step = 0.05
      -- Final time of integration.
      t_final = 1.0

      -- The objective function which we're aiming to minimise.
      objective_fn :: (Reifies s W) => State -> BVar s Net -> BVar s Double
      objective_fn state net =
        loss $
          integrate
            t_step
            t_final
            (sysDyn config net)
            (constVar state)

      -- \| List of the gradients; one for each entry in the batch.
      batch_gradients :: [L.Grad Net]
      batch_gradients =
        batch <&> \(state :: State) ->
          L.Grad $ gradBP (objective_fn state) init_net

      -- \| The mean gradient; averaged across the batch.
      mean_gradient :: L.Grad Net
      mean_gradient = listMean batch_gradients

      -- \| Apply Adam learning update.
      (net', adam_bufs') =
        L.adamUpdate adam_config init_adam_bufs mean_gradient init_net
      net'' = L.decayWeights 0.001 net'
   in (net'', adam_bufs')

train ::
  -- | Configuration of the cartpole system.
  CartPoleConfig ->
  -- | Batch size.
  Int ->
  -- | Starting state for a given iteration.
  (Int -> State) ->
  -- | Configuration for Adam learning.
  AdamConfig Double ->
  -- | Initial controller network parameters.
  Net ->
  -- | Evolution of the network during training.
  [Net]
train config batch_size mk_state adam_config init_net =
  let init_adam_bufs :: AdamBufs Net
      init_adam_bufs = L.zeroAdamBufs

      trainLoop :: Int -> AdamBufs Net -> Net -> [Net]
      trainLoop index adam_bufs net =
        let states = take batch_size $ mk_state <$> [index ..]
            (net', adam_bufs') =
              trainStepBatch
                config
                states
                adam_config
                adam_bufs
                net
         in net' : trainLoop (index + batch_size) adam_bufs' net'
   in trainLoop 0 init_adam_bufs init_net

---- Training in Phases -------------------------------------------------------

trainSwingup ::
  CartPoleConfig ->
  Int ->
  AdamConfig Double ->
  Net ->
  [Net]
trainSwingup config batch_size = train config batch_size mk_state
  where
    mk_state i = angleToState $ (\x -> x * pi / 8) $ frameRand i

trainBalance ::
  CartPoleConfig ->
  Int ->
  AdamConfig Double ->
  Net ->
  [Net]
trainBalance config batch_size = train config batch_size mk_state
  where
    mk_state i = angleToState $ (\x -> pi - x * pi / 8) $ frameRand i

trainBoth ::
  CartPoleConfig ->
  Int ->
  AdamConfig Double ->
  Net ->
  [Net]
trainBoth config batch_size = train config batch_size mk_state
  where
    mk_state i = angleToState $ (\x -> pi * x) $ frameRand i

trainFull :: [Net]
trainFull =
  let config = defaultCartPoleConfig

      swingup_n = 200
      swingup_adam = L.defaultAdamConfig (Proxy :: Proxy Net)
      swingup_nets =
        take swingup_n $
          trainSwingup config 1 swingup_adam initNet

      balance_n = 200
      balance_adam = L.defaultAdamConfig (Proxy :: Proxy Net)
      balance_nets =
        take balance_n $
          trainBalance config 4 balance_adam (last swingup_nets)

      both_n = 200
      both_adam = L.defaultAdamConfig (Proxy :: Proxy Net)
      both_nets =
        take both_n $
          trainBoth config 8 both_adam (last balance_nets)

      su1 =
        take swingup_n $
          trainSwingup config 1 swingup_adam initNet
      b1 =
        take balance_n $
          trainBalance config 4 balance_adam (last su1)
      su2 =
        take swingup_n $
          trainSwingup config 1 swingup_adam (last b1)
      b2 =
        take balance_n $
          trainBalance config 4 balance_adam (last su2)
      a =
        take both_n $
          trainBoth config 8 both_adam (last b2)

      training_nets = concat [su1, b1, su2, b2, a]
   in training_nets

trainBimodal :: [Net]
trainBimodal =
  let batch_size = 8
      n_frames = 10000

      mk_state i =
        let max_ang
              | i < 500 = pi / 16
              | i < 1000 = pi * fromIntegral (i - 500) / 500
              | otherwise = pi
         in angleToState $ frameRandRange 0 max_ang i

      def_adam_config = L.defaultAdamConfig (Proxy :: Proxy Net)
      adam_config = def_adam_config {L.adamConfigLR = 0.005}
   in take n_frames $
        train defaultCartPoleConfig batch_size mk_state adam_config initNet

angleToState :: Double -> State
angleToState angle = State $ fromList [0, 0, angle, 0, 0]

frameRandRange :: Double -> Double -> Int -> Double
frameRandRange min_val max_val frame_number =
  let range_val = max_val - min_val
   in frameRand frame_number * range_val + min_val

-- | Generate a uniform random number from 0 to 1 based on a frame number.
frameRand :: Int -> Double
frameRand = fst . random . mkStdGen

-- | Generate a random number drawn from a Normal distribution.
frameNormal ::
  -- | Mean.
  Double ->
  -- | Standard deviation.
  Double ->
  -- | Frame number.
  Int ->
  -- | Random number.
  Double
frameNormal mean stdev frame_number =
  let z = frameStdNormal frame_number
   in z * stdev + mean

-- | Generate a random number drawn from a standard Normal distribution,
--   based on a frame number.
frameStdNormal :: Int -> Double
frameStdNormal frame_number =
  let gen = mkStdGen frame_number
      (u1, g1) = random gen
      (u2, _) = random g1
      z = sqrt (-2 * log u1) * cos (2 * pi * u2)
   in z

---- Plotting -----------------------------------------------------------------

initPlotStates :: [State]
initPlotStates =
  let n_angles = 25
      itod = fromIntegral :: Int -> Double
      angles = [0 .. n_angles] <&> \i -> itod i * pi / itod n_angles
   in angleToState <$> angles

fwdTrajectory :: CartPoleConfig -> Net -> State -> [(Double, State)]
fwdTrajectory config net init_state =
  let t_step = 0.05
      t_final = 2.0
   in evalBP
        (integrate t_step t_final (sysDyn config (constVar net)))
        init_state

trajGetTime :: [(Double, State)] -> [Double]
trajGetTime traj = fst <$> traj

trajGetAngle :: [(Double, State)] -> [Double]
trajGetAngle traj = stateTheta . snd <$> traj

trajGetTimeAngle :: [(Double, State)] -> [(Double, Double)]
trajGetTimeAngle traj = zip (trajGetTime traj) (trajGetAngle traj)

trajectoriesFromInitStates :: CartPoleConfig -> Net -> [[(Double, State)]]
trajectoriesFromInitStates config net =
  fwdTrajectory config net <$> initPlotStates

----

renderAnimation ::
  Logger ->
  Path Abs Dir ->
  IO ()
renderAnimation logger outdir = do
  let config = defaultCartPoleConfig

      file_names :: [Path Abs File]
      file_names = [0 ..] <&> \i -> outdir </> relFileNum 6 i ".png"

      actions :: [IO ()]
      actions =
        zip file_names trainBimodal
          <&> \(outfile, net) -> renderFrame logger outfile config net
  run actions

renderFrame ::
  Logger ->
  Path Abs File ->
  CartPoleConfig ->
  Net ->
  IO ()
renderFrame logger outfile config net = do
  let trajectories = trajectoriesFromInitStates config net
      timeAngle = trajGetTimeAngle <$> trajectories
  logString logger $ "Rendering: " <> Path.toFilePath outfile
  toFile (FileOptions (640, 480) PNG) (Path.toFilePath outfile) $ do
    CE.plot (CE.line "angle" timeAngle)
  pure ()

cartpoleLearningAnimation :: Logger -> Path Abs Dir -> IO ()
cartpoleLearningAnimation logger out_dir = do
  let n_frames = 200 :: Int

      config :: CartPoleConfig
      config = defaultCartPoleConfig

      adam_config :: AdamConfig Double
      adam_config =
        (L.defaultAdamConfig (Proxy :: Proxy Net))
          { L.adamConfigLR = 0.001
          }

      mk_state :: Int -> State
      mk_state frame =
        let rand_f :: Double
            rand_f = fst (random (mkStdGen frame))
            -- rand_angle = pi  - (0.3 * pi * rand_f)
            rand_angle = pi * rand_f
         in State $ fromList [0, 0, rand_angle, 0, 0]

      init_net :: Net
      init_net = initNet

      batch_size :: Int
      batch_size = 8

      nets :: [Net]
      nets = train config batch_size mk_state adam_config init_net

      file_names :: [Path Abs File]
      file_names = [0 ..] <&> \i -> out_dir </> relFileNum 6 i ".png"

      actions :: [IO ()]
      actions =
        take n_frames $
          zip file_names nets
            <&> uncurry (renderCartpoleLearningAnimationFrame logger)

  run actions

renderCartpoleLearningAnimationFrame ::
  Logger ->
  Path Abs File ->
  Net ->
  IO ()
renderCartpoleLearningAnimationFrame logger outfile net = do
  let t_step = 0.02
      t_final = 2.0

      config = defaultCartPoleConfig

      traj_fn :: State -> [(Double, State)]
      traj_fn = evalBP (integrate t_step t_final (sysDyn config (auto net)))

      trajs :: [[(Double, State)]]
      trajs = traj_fn <$> init_states

      get_ts :: [(Double, State)] -> [Double]
      get_ts traj = fst <$> traj

      get_xs :: [(Double, State)] -> [Double]
      get_xs traj = stateX . snd <$> traj

      get_angles :: [(Double, State)] -> [Double]
      get_angles traj = stateTheta . snd <$> traj

      x_trajectories = (\traj -> zip (get_ts traj) (get_xs traj)) <$> trajs
      angle_trajectories = (\traj -> zip (get_ts traj) (get_angles traj)) <$> trajs

      n_angles = 25
      init_angles =
        [0 .. n_angles] <&> \(i :: Int) ->
          fromIntegral i * pi / fromIntegral n_angles
      init_states = (\angle -> State (fromList [0, 0, angle, 0, 0])) <$> init_angles

      zero_angles = [[(0, pi), (2, pi)]]

      theta_line :: C.PlotLines Double Double
      theta_line =
        C.plot_lines_values .~ angle_trajectories $
          def

      x_lines :: C.PlotLines Double Double
      x_lines =
        C.plot_lines_values .~ x_trajectories $
          def

      zero_angle_lines :: C.PlotLines Double Double
      zero_angle_lines =
        C.plot_lines_values .~ zero_angles $
          C.plot_lines_style . C.line_color .~ opaque grey $
            C.plot_lines_style . C.line_dashes .~ [4, 4] $
              def

      layout_angles :: C.Layout Double Double
      layout_angles =
        C.layout_plots
          .~ [ C.toPlot zero_angle_lines,
               C.toPlot theta_line
             ]
          $ C.layout_y_axis
            . C.laxis_generate
            .~ C.scaledAxis
              def
              (-2.2 * pi, 2.2 * pi)
          $ def

      layout_xs :: C.Layout Double Double
      layout_xs =
        C.layout_plots
          .~ [ C.toPlot x_lines
             ]
          $ def

      grid = CL.layoutToGrid layout_angles `CG.above` CL.layoutToGrid layout_xs

      chart :: C.Renderable ()
      chart = C.toRenderable grid

  logString logger $ "Rendering: " <> Path.toFilePath outfile
  let opts = FileOptions (640, 480) PNG
  _ <- renderableToFile opts (Path.toFilePath outfile) chart
  pure ()

-}

---- Episodic Training --------------------------------------------------------

-- | Square a value.
sq :: (Floating s) => s -> s
sq x = x ^ (2 :: Int)

-- | Convert degress to radians.
degToRad :: (Floating s) => s -> s
degToRad x = x * pi / 180

-- | Run an episode of training.
--
-- A training episode runs until either the maximum time has elapsed, or
-- until the pole is considered to be sufficiently balanced.
runEpisode ::
  forall s.
  (Reifies s W) =>
  -- | Configuration of the system.
  CartPoleConfig ->
  -- | Time step (seconds).
  Double ->
  -- | Maximum time to integrate (seconds).
  Double ->
  -- | Controller parameters.
  BVar s Net ->
  -- | Initial state.
  State ->
  -- | Trajectory.
  [(BVar s Double, BVar s State)]
runEpisode config t_step t_max net init_state =
  let {-balancedEnough :: BVar s State -> Bool
      balancedEnough (BV (State _x _xd theta theta_dot _e)) =
        let theta_threshold = degToRad 1
            theta_dot_threshold = degToRad 1

            theta_ok = abs (pi - theta) < theta_threshold
            theta_dot_ok = abs theta_dot < theta_dot_threshold
         in theta_ok && theta_dot_ok
         -}

      stop_fn :: BVar s Double -> BVar s State -> Bool
      stop_fn t _ = t >= constVar t_max

      grad_fn :: BVar s State -> Grad (BVar s State)
      grad_fn = sysDyn config net
   in odeIntRK4
        (constVar t_step)
        (const grad_fn)
        stop_fn
        (0, constVar init_state)

-- | Find the minimum of a list according to some ordering function.

{-
minimumBy :: forall a b. (Ord b) => (a -> b) -> [a] -> a
minimumBy _ [] = error "Cannot find the minimum of an empty list."
minimumBy _ [x] = x
minimumBy f (x : xs) = go x (f x) xs
  where
    go :: a -> b -> [a] -> a
    go min_item _ [] = min_item
    go min_item min_val (x' : xs') =
      let trial_val = f x'
       in if trial_val < min_val
            then go x' trial_val xs'
            else go min_item min_val xs'
-}

-- | Find the most balanced point in a list of states.

{-
mostBalancedPoint :: forall s. (Reifies s W) => [BVar s State] -> BVar s State
mostBalancedPoint = minimumBy balance_measure . collectVar . drop 4
  where
    balance_measure :: State -> Double
    balance_measure (State _x _xd theta theta_dot _e) =
      sq (theta - pi) + 0.1 * sq theta_dot
-}

pathLoss :: forall s. (Reifies s W) => [BVar s State] -> BVar s Double
pathLoss states =
  listMean $
    states <&> \state ->
      let State x x_dot theta theta_dot effort = splitBV state
          loss_x = sq x
          loss_x_dot = sq x_dot
          loss_theta = sq (pi - theta)
          loss_theta_dot = sq theta_dot
          loss_effort = sq effort

          w_x = 0.1
          w_x_dot = 0.05
          w_theta = 1.0
          w_theta_dot = 0.5
          w_effort = 0.05
       in w_x * loss_x
            + w_x_dot * loss_x_dot
            + w_theta * loss_theta
            + w_theta_dot * loss_theta_dot
            + w_effort * loss_effort

-- | Compute the loss for a trajectory.
terminalLoss :: forall s. (Reifies s W) => [BVar s State] -> BVar s Double
terminalLoss states =
  let State x x_dot theta theta_dot effort = splitBV $ last states

      loss_x = sq x
      loss_x_dot = sq x_dot
      loss_theta = sq (pi - theta)
      loss_theta_dot = sq theta_dot
      loss_effort = sq effort

      w_x = 0.1
      w_x_dot = 0.05
      w_theta = 1.0
      w_theta_dot = 0.5
      w_effort = 0.05
   in w_x * loss_x
        + w_x_dot * loss_x_dot
        + w_theta * loss_theta
        + w_theta_dot * loss_theta_dot
        + w_effort * loss_effort

-- Loss below works...
{-
loss :: forall s. (Reifies s W) => [BVar s State] -> BVar s Double
loss states =
  listMean $ states <&> \s ->
    let State _ _ theta _ _ = splitBV s
    in theta * theta
-}

-- | Perform episoding training on a single batch.
trainBatchEpisodic ::
  CartPoleConfig ->
  [State] ->
  AdamConfig Double ->
  (Net, AdamBufs Net) ->
  (Net, AdamBufs Net)
trainBatchEpisodic config batch adam_config (init_net, adam_bufs) =
  let t_step = 0.1
      t_max = 5.0

      -- Objective function for an episode.
      objective_fn :: forall s. (Reifies s W) => State -> BVar s Net -> BVar s Double
      objective_fn init_state net =
        let paths =
              snd
                <$> runEpisode
                  config
                  t_step
                  t_max
                  net
                  init_state
         in terminalLoss paths + 0.1 * pathLoss paths

      -- Gradients resulting from all items in the batch.
      grads :: [L.Grad Net]
      grads =
        batch <&> \init_state ->
          L.Grad $ gradBP (objective_fn init_state) init_net

      -- Average gradient.
      avg_grad :: L.Grad Net
      avg_grad = listMean grads
   in L.adamUpdate adam_config adam_bufs avg_grad init_net

-- 'snd' lifted to operate over a tuple inside a 'BVar'.
bvarSnd ::
  (Backprop a, Backprop b, Reifies s W) =>
  BVar s (a, b) ->
  BVar s b
bvarSnd (T2 _ y) = y
bvarSnd _ = error "This case should not occur"

-- | Perform episodic training; producing a list of progressively better
--   trained networks.
trainEpisodic ::
  CartPoleConfig ->
  Int ->
  (Int -> State) ->
  AdamConfig Double ->
  Net ->
  [Net]
trainEpisodic config batch_size mk_init_state adam_config init_net =
  let trainLoop :: Int -> AdamBufs Net -> Net -> [Net]
      trainLoop index adam_bufs net =
        let batch = take batch_size $ mk_init_state <$> [index ..]
            (net', adam_bufs') =
              trainBatchEpisodic config batch adam_config (net, adam_bufs)
         in net' : trainLoop (index + batch_size) adam_bufs' net'
   in trainLoop 0 L.zeroAdamBufs init_net

trainEpisodicRun :: [Net]
trainEpisodicRun =
  let batch_size = 4
      n_frames = 50000
   in take n_frames $
        trainEpisodic
          defaultCartPoleConfig
          batch_size
          ( \i ->
              State
                0
                0
                (frameRandRange 0 (pi / 4) i)
                (frameRandRange (-0.1 * pi) (0.1 * pi) i)
                0
          )
          (L.defaultAdamConfig (Proxy :: Proxy Net))
          initNet

frameRandRange :: Double -> Double -> Int -> Double
frameRandRange min_val max_val frame_number =
  let range_val = max_val - min_val
   in frameRand frame_number * range_val + min_val

frameRand :: Int -> Double
frameRand = fst . random . mkStdGen

---- Plotting -----------------------------------------------------------------

initPlotStates :: [State]
initPlotStates =
  let n_angles = 25
      itod = fromIntegral :: Int -> Double
      angles = [0 .. n_angles] <&> \i -> itod i * pi / itod n_angles
   in (\theta -> State 0 0 theta 0 0) <$> angles

fwdTrajectory :: CartPoleConfig -> Net -> State -> [(Double, State)]
fwdTrajectory config net init_state =
  let t_step = 0.1
      t_final = 5.0
   in evalBP
        (integrate t_step t_final (sysDyn config (constVar net)))
        init_state

trajGetTime :: [(Double, State)] -> [Double]
trajGetTime traj = fst <$> traj

trajGetAngle :: [(Double, State)] -> [Double]
trajGetAngle = fmap (\(_, State _ _ angle _ _) -> angle)

trajGetTimeAngle :: [(Double, State)] -> [(Double, Double)]
trajGetTimeAngle traj = zip (trajGetTime traj) (trajGetAngle traj)

trajectoriesFromInitStates :: CartPoleConfig -> Net -> [[(Double, State)]]
trajectoriesFromInitStates config net =
  fwdTrajectory config net <$> initPlotStates

----

renderAnimation ::
  Logger ->
  Path Abs Dir ->
  IO ()
renderAnimation logger outdir = do
  let config = defaultCartPoleConfig

      file_names :: [Path Abs File]
      file_names = [0 ..] <&> \i -> outdir </> relFileNum 6 i ".png"

      actions :: [IO ()]
      actions =
        zip file_names trainEpisodicRun
          <&> \(outfile, net) -> renderFrame logger outfile config net
  run actions

renderFrame ::
  Logger ->
  Path Abs File ->
  CartPoleConfig ->
  Net ->
  IO ()
renderFrame logger outfile config net = do
  let trajectories = trajectoriesFromInitStates config net
      timeAngle = trajGetTimeAngle <$> trajectories
  logString logger $ "Rendering: " <> Path.toFilePath outfile
  toFile (FileOptions (640, 480) PNG) (Path.toFilePath outfile) $ do
    C.layout_y_axis . C.laxis_generate .= C.scaledAxis def (-0.5, 3.5)
    CE.plot (CE.line "angle" timeAngle)
  pure ()

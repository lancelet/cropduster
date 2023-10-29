{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Duster.MassSpringDamper where

import Control.Concurrent (newMVar, putMVar, takeMVar)
import Control.Monad (forM_)
import Data.VectorSpace (AdditiveGroup, VectorSpace, (*^), (^-^))
import Duster.LinearFit (relFileNum)
import Duster.ODE (Grad (Grad), odeIntRK4, termt)
import Duster.Orphans ()
import GHC.Generics (Generic)
import Graphics.Matplotlib (Matplotlib, (%))
import qualified Graphics.Matplotlib as Plt
import Lens.Micro ((^.), _2)
import Lens.Micro.TH (makeLenses)
import Numeric.Backprop
  ( BVar,
    Backprop,
    Reifies,
    W,
    auto,
    collectVar,
    evalBP,
    gradBP,
    sequenceVar,
    (^^.),
    pattern T2,
  )
import Numeric.LinearAlgebra ((!))
import Numeric.LinearAlgebra.Static (Sized (extract), fromList)
import Numeric.LinearAlgebra.Static.Backprop
  ( L,
    R,
    dot,
    matrix,
    vec2,
    (#>),
  )
import Path (Abs, Dir, Path, (</>))
import qualified Path

type State = R 2

stateX :: State -> Double
stateX state = extract state ! 0

stateXDot :: State -> Double
stateXDot state = extract state ! 1

data Param = Param
  { _param_m :: Double,
    _param_k :: Double,
    _param_c :: Double
  }
  deriving (Generic, Show, AdditiveGroup, VectorSpace)

instance Backprop Param

makeLenses ''Param

gradFn ::
  forall s.
  (Reifies s W) =>
  BVar s Param ->
  BVar s Double ->
  BVar s State ->
  Grad (BVar s State)
gradFn ps _t state = Grad $ mm #> state
  where
    m, k, c :: BVar s Double
    m = ps ^^. param_m
    k = ps ^^. param_k
    c = ps ^^. param_c

    mm :: BVar s (L 2 2)
    mm = matrix [0, 1, -k / m, -c / m]

integ1 :: IO ()
integ1 = do
  let s0 :: R 2
      s0 = fromList [1, 0]

      param :: BVar s Param
      param = auto $ Param 1 50 2

      states ::
        forall s.
        (Reifies s W) =>
        (BVar s Double, BVar s (R 2)) ->
        [(BVar s Double, BVar s (R 2))]
      states = odeIntRK4 0.02 (gradFn param) (termt 5)

      unComb2 ::
        (Backprop a, Backprop b, Reifies s W) =>
        BVar s (a, b) ->
        (BVar s a, BVar s b)
      unComb2 (T2 x y) = (x, y)
      unComb2 _ = error "Unexpected case"

      comb2 ::
        (Backprop a, Backprop b, Reifies s W) =>
        (BVar s a, BVar s b) ->
        BVar s (a, b)
      comb2 (x, y) = T2 x y

      combL2 ::
        (Reifies s W, Backprop a, Backprop b) =>
        [(BVar s a, BVar s b)] ->
        BVar s [(a, b)]
      combL2 = collectVar . fmap comb2

      statesB :: (Reifies s W) => BVar s (Double, R 2) -> BVar s [(Double, R 2)]
      statesB = combL2 . states . unComb2

      evStates :: [(Double, R 2)]
      evStates = evalBP statesB (0, s0)

      ts :: [Double]
      ts = fst <$> evStates

      xs :: [Double]
      xs = flip (!) 0 . extract . snd <$> evStates

      plot = Plt.plot ts xs

  Plt.onscreen plot

integ2 :: Path Abs Dir -> IO ()
integ2 out_dir = do
  let h, t_final :: (Fractional a) => a
      h = 0.02
      t_final = 5.0

      ref_param :: Param
      ref_param = Param 1 50 2

      init_state :: (Double, R 2)
      init_state = (0, fromList [1, 0])

      integrate' ::
        (Reifies s W) =>
        BVar s Param ->
        [(BVar s Double, BVar s (R 2))]
      integrate' param =
        odeIntRK4 h (gradFn param) (termt t_final) (liftTuple init_state)

      integrate ::
        forall s.
        (Reifies s W) =>
        BVar s Param ->
        BVar s [(Double, R 2)]
      integrate = collectVar . fmap combine2T . integrate'

      ref_result :: [(Double, R 2)]
      ref_result = evalBP integrate ref_param

      ref_ts, ref_xs :: [Double]
      ref_ts = fst <$> ref_result
      ref_xs = stateX . snd <$> ref_result

      calc_loss ::
        (Reifies s W) =>
        [Double] ->
        [BVar s Double] ->
        BVar s Double
      calc_loss ref comp = sum $ sq <$> zipWith (-) (auto <$> ref) comp

      sq :: (Num a) => a -> a
      sq x = x * x

      fwd :: forall s. (Reifies s W) => BVar s Param -> BVar s Double
      fwd param = calc_loss ref_xs $ getX . (^^. _2) <$> sequenceVar result
        where
          result :: BVar s [(Double, R 2)]
          result = integrate param

          getX :: BVar s (R 2) -> BVar s Double
          getX state = dot state (vec2 1 0)

  paramMVar <- newMVar $ Param 2.0 1.0 0.0

  forM_ [0 .. 180] $ \(index :: Int) -> do
    param <- takeMVar paramMVar
    let outfile = out_dir </> relFileNum 4 index ".png"

        result :: [(Double, R 2)]
        result = evalBP integrate param

        grad :: Param
        grad = gradBP fwd param

        ts, xs :: [Double]
        ts = fst <$> result
        xs = stateX . snd <$> result

        lr :: Double
        lr = 5e-3

        param' :: Param
        param' = param ^-^ lr *^ grad

        param'' :: Param
        param'' = Param m' k' c'
          where
            m = param' ^. param_m
            k = param' ^. param_k
            c = param' ^. param_c

            m_min = 0.1
            k_min = 0.1

            m' = max m m_min
            k' = max k k_min
            c' = max c 0

    putMVar paramMVar param''

    let plot :: Matplotlib
        plot =
          Plt.plot ref_ts ref_xs
            % Plt.plot ts xs
            % Plt.xlabel "time (s)"
            % Plt.ylabel "displacement"
            % Plt.title "Mass-spring-damper tuning"

    putStrLn $ "Rendering file: " <> Path.toFilePath outfile
    Plt.file (Path.toFilePath outfile) plot

liftTuple :: (a, b) -> (BVar s a, BVar s b)
liftTuple (x, y) = (auto x, auto y)

combine2T ::
  ( Backprop a,
    Backprop b,
    Reifies s W
  ) =>
  (BVar s a, BVar s b) ->
  BVar s (a, b)
combine2T (x, y) = T2 x y

unCombine2T ::
  ( Backprop a,
    Backprop b,
    Reifies s W
  ) =>
  BVar s (a, b) ->
  (BVar s a, BVar s b)
unCombine2T (T2 x y) = (x, y)
unCombine2T _ = error "unexpected input"

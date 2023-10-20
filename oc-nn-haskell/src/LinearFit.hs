{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module LinearFit
  ( -- * Functions
    linearFittingAnimation,
    lossLandscapeAnimation,
  )
where

import Control.Exception (Exception (displayException), assert)
import Control.Monad (forM_)
import Data.Bifunctor (second)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Data.VectorSpace
  ( AdditiveGroup (zeroV),
    Scalar,
    VectorSpace,
    (*^),
    (^+^),
    (^-^),
    (^/),
  )
import GHC.Generics (Generic)
import Graphics.Matplotlib ((%), (@@))
import qualified Graphics.Matplotlib as Plt
import Path (Abs, Dir, File, Path, Rel, (</>))
import qualified Path
import Statistics.Distribution (ContDistr, quantile)
import Statistics.Distribution.Normal (NormalDistribution, normalDistr)
import System.Random (Random, RandomGen, mkStdGen, randoms, split)
import Text.Printf (printf)

-- | 2D Point.
data Pt = Pt
  { pt_x :: Float,
    pt_y :: Float
  }

-- | Convert a `Pt` to an `Example`.
ptToExample :: Pt -> Example Float Float
ptToExample (Pt x y) = Example x y

-- | Parameters of the equation for a straight line.
--
-- 'y = theta_1 * x + theta_0'.
data Line = Line
  { -- | y-intercept of the line (ie. coefficient for input raised to zero).
    theta_0 :: Float,
    -- | slope of the line (ie. coefficient for input raised to first power).
    theta_1 :: Float
  }
  deriving (Show, Generic, AdditiveGroup, VectorSpace)

-- | Given parameters of a linear relationship, apply the equation for the
--   line to an input.
linear :: Line -> Float -> Float
linear (Line c m) x = m * x + c

---- SGD ----------------------------------------------------------------------

-- | Mark a type as being a gradient.
newtype Grad t = Grad t deriving (Generic, AdditiveGroup, VectorSpace)

-- | Perform a gradient descent update of a value.
--
-- This is a single step of SGD learning.
sgdUpdate ::
  (VectorSpace t) =>
  -- | Learning rate.
  Scalar t ->
  -- | Initial value.
  t ->
  -- | Gradient of the value.
  Grad t ->
  -- | New value, after gradient descent update.
  t
sgdUpdate r theta (Grad dtheta) = theta ^-^ (r *^ dtheta)

-- | Example for supervised training.
data Example i o = Example
  { -- | Input value.
    example_input :: i,
    -- | Expected result.
    example_output :: o
  }
  deriving (Eq)

-- | Batch of paired inputs and outputs for supervised training.
type Batch i o = [Example i o]

-- | Loss function with derivative.
--
-- A loss function takes a batch of examples along with the current learnable
-- parameters and returns a tuple containing the value of the loss and the
-- gradient of loss with respect to all the parameters.
type LossFnD i o t = Batch i o -> t -> (Float, Grad t)

-- | Perform a learning step on a batch of examples, updating parameters to
--   their new values and returning the loss and new parameters.
--
-- A learning step combines calculating the loss with an SGD update of the
-- parameters.
learningStep ::
  forall i o t.
  (VectorSpace t) =>
  -- | Learning rate.
  Scalar t ->
  -- | Loss function.
  LossFnD i o t ->
  -- | Current parameter(s).
  t ->
  -- | Batch of examples.
  Batch i o ->
  -- | Loss and updated parameter(s).
  (Float, t)
learningStep r lf t batch = second (sgdUpdate r t) (lf batch t)

-- | Fit a sequence of batches.
--
-- Fitting runs multiple learning steps across all batches.
fit ::
  forall i o t.
  (VectorSpace t) =>
  -- | Learning rate.
  Scalar t ->
  -- | Loss function.
  LossFnD i o t ->
  -- | List of batches to process.
  [Batch i o] ->
  -- | Initial parameter.
  t ->
  -- | List of loss and updated parameters after each batch.
  [(Float, t)]
fit r lf batches t = scanl step (initLoss, t) batches
  where
    initLoss :: Float
    initLoss = fst $ lf (head batches) t

    step :: (Float, t) -> Batch i o -> (Float, t)
    step (_, p) = learningStep r lf p

---- Linear Fit ---------------------------------------------------------------

-- | Loss function for a linear fit.
--
-- The loss function takes a batch of examples, and returns the least-squares
-- difference between predicted values and actual values. It also returns the
-- gradient of this value with respect to each parameter of the line.
linfitLossFn :: LossFnD Float Float Line
linfitLossFn egs line = (mean (egLoss <$> egs), mean (egGradLoss <$> egs))
  where
    egLoss :: Example Float Float -> Float
    egLoss (Example x y) = (linear line x - y) ** 2

    egGradLoss :: Example Float Float -> Grad Line
    egGradLoss (Example x y) = Grad (Line dTheta_0 dTheta_1)
      where
        y' = linear line x
        dTheta_0 = 2 * (y' - y)
        dTheta_1 = 2 * (y' - y) * x

-- | Compute the mean of a list.
mean :: forall v s. (VectorSpace v, s ~ Scalar v, Fractional s) => [v] -> v
mean [] = error "Cannot compute mean of an empty list."
mean xs = go 0 zeroV xs
  where
    go :: (Fractional s, s ~ Scalar v) => Int -> v -> [v] -> v
    go n run_sum [] = run_sum ^/ fromIntegral n
    go n run_sum (y : ys) = go (n + 1) (run_sum ^+^ y) ys

---- Animated sequence generation ---------------------------------------------

-- | Perform a linear fit for animation.
--
-- The output from this function produces a list of tuples containing the
-- batch used for a training step along with
fitForAnimation ::
  -- | Number of epochs to train.
  Int ->
  -- | Batch size.
  Int ->
  -- | Learning rate.
  Float ->
  -- | List of tuples of the batch used, and the loss and parameters it
  --   produced.
  [(Maybe (Batch Float Float), Float, Line)]
fitForAnimation n_epochs batch_size lr =
  let init_line = Line 10 -1.2

      dataset :: [Example Float Float]
      dataset = concat $ replicate n_epochs $ ptToExample <$> defaultLinFitPts

      batches :: [Batch Float Float]
      batches = chunksOf batch_size dataset

      fits :: [(Float, Line)]
      fits = fit lr linfitLossFn batches init_line

      convert :: (m, (l, t)) -> (m, l, t)
      convert (m, (l, t)) = (m, l, t)
   in convert <$> zip (Nothing : (Just <$> batches)) fits

-- | Produce an animation of linear fitting for demonstration purposes.
--
-- This calls matplotlib, outputting individual PNG files showing the
-- progress of linear fitting via SGD.
linearFittingAnimation ::
  -- | Parent directory for output of PNG files. This must exist.
  Path Abs Dir ->
  -- | Maximum number of frames to render.
  --
  -- The actual number of rendered frames may be less than this depending on the
  -- dataset size, batch size and number of epochs.
  Int ->
  -- | Batch size to use.
  Int ->
  -- | Learning rate.
  Float ->
  -- | IO action to create the animation.
  IO ()
linearFittingAnimation out_dir max_frames batch_size lr = do
  let n_epochs = 4 :: Int

      lin_fit_examples :: [Example Float Float]
      lin_fit_examples = ptToExample <$> defaultLinFitPts

      batch_and_outcome :: [(Maybe (Batch Float Float), Float, Line)]
      batch_and_outcome = fitForAnimation n_epochs batch_size lr

      llsq :: Line
      llsq = lsqFit defaultLinFitPts
   in forM_ (zip [0 .. (max_frames - 1)] batch_and_outcome) $
        \(index :: Int, (maybe_batch, _loss, line)) -> do
          let outfile = out_dir </> relFileNum 4 index ".png"

              batch_examples :: [Example Float Float]
              batch_examples = fromMaybe [] maybe_batch

              bg_examples :: [Example Float Float]
              bg_examples = filter (`notElem` batch_examples) lin_fit_examples

              xs_bg = fmap example_input bg_examples
              ys_bg = fmap example_output bg_examples

              xs_batch = fmap example_input batch_examples
              ys_batch = fmap example_output batch_examples

              x_min = -6 :: Float
              x_max = 6 :: Float
              plot =
                Plt.scatter xs_bg ys_bg
                  % Plt.scatter xs_batch ys_batch
                    @@ [Plt.o2 "color" "red"]
                  % Plt.line
                    [x_min, x_max]
                    [linear line x_min, linear line x_max]
                  % Plt.plot
                    [x_min, x_max]
                    [linear llsq x_min, linear llsq x_max]
                    @@ [Plt.o2 "ls" "--"]
                  % Plt.xlim @Double @Double
                    (realToFrac x_min)
                    (realToFrac x_max)
                  % Plt.ylim @Double @Double -4 13
          putStrLn $ "Rendering file: " <> Path.toFilePath outfile
          Plt.file (Path.toFilePath outfile) plot

-- | Produce an animation of the loss landscape during linear fitting for
--   demonstration purposes.
--
-- This calls matplotlib, outputting individual PNG files showing the
-- progress of linear fitting via SGD.
lossLandscapeAnimation ::
  Path Abs Dir ->
  -- | Batch size to use.
  Int ->
  -- | Learning rate.
  Float ->
  -- | IO action to create the animation.
  IO ()
lossLandscapeAnimation out_dir batch_size lr = do
  let -- Training trajectory

      n_epochs = 12 :: Int

      batch_and_outcome :: [(Maybe (Batch Float Float), Float, Line)]
      batch_and_outcome = fitForAnimation n_epochs batch_size lr

      llsq :: Line
      llsq = lsqFit defaultLinFitPts

      -- Loss landscape background

      all_pts :: [Example Float Float]
      all_pts = fmap ptToExample defaultLinFitPts

      loss_landscape :: Line -> Float
      loss_landscape = fst . linfitLossFn all_pts

      theta0_min = 4.0 :: Float
      theta0_max = 11.0 :: Float
      theta1_min = -1.5 :: Float
      theta1_max = 2.0 :: Float

      extent :: [Double]
      extent = fmap realToFrac [theta0_min, theta0_max, theta1_min, theta1_max]

      tabulated_loss_landscape :: [[Float]]
      tabulated_loss_landscape =
        let n_theta0 = 50 :: Int
            n_theta1 = 50 :: Int

            j_to_theta0 :: Int -> Float
            j_to_theta0 j =
              theta0_min
                + (fromIntegral j / fromIntegral (n_theta0 - 1))
                  * (theta0_max - theta0_min)

            i_to_theta1 :: Int -> Float
            i_to_theta1 i =
              theta1_min
                + (fromIntegral i / fromIntegral (n_theta1 - 1))
                  * (theta1_max - theta1_min)

            lossij :: Int -> Int -> Float
            lossij i j = loss_landscape (Line (j_to_theta0 j) (i_to_theta1 i))
         in [[lossij i j | j <- [0 .. n_theta1]] | i <- [0 .. n_theta0]]

  forM_ (zip [0 ..] batch_and_outcome) $
    \(index :: Int, _) -> do
      let outfile = out_dir </> relFileNum 4 index ".png"

          ls :: [Line]
          ls = (\(_, _, x) -> x) <$> take (index + 1) batch_and_outcome

          plot =
            Plt.imshow tabulated_loss_landscape
              @@ [ Plt.o2 "extent" extent,
                   Plt.o2 "origin" "lower",
                   Plt.o2 "aspect" "auto",
                   Plt.o2 "interpolation" "bilinear"
                 ]
              % Plt.plot (theta_0 <$> ls) (theta_1 <$> ls)
                @@ [ Plt.o2 "ls" "--",
                     Plt.o2 "color" "orange"
                   ]
              % Plt.scatter (theta_0 <$> ls) (theta_1 <$> ls)
              % Plt.scatter [theta_0 llsq] [theta_1 llsq]
                @@ [ Plt.o2 "color" "red",
                     Plt.o2 "marker" "x"
                   ]
              % Plt.xlabel "y-intercept"
              % Plt.ylabel "slope"
      putStrLn $ "Rendering file: " <> Path.toFilePath outfile
      Plt.file (Path.toFilePath outfile) plot

-- | Construct a numbered relative file.
--
-- eg:
-- >>> relFileNum 5 42 ".png"
-- "00042.png"
relFileNum ::
  -- Width of numeric part of the file name.
  Int ->
  -- Number of the file name.
  Int ->
  -- Extension of the filename (including the dot).
  String ->
  -- Relative path to a file.
  Path Rel File
relFileNum width n ext = relFile $ printf ("%0*d" <> ext) width n

-- | Construct a relative file, throwing a runtime error if that is not
--   possible.
relFile :: String -> Path Rel File
relFile name =
  case Path.parseRelFile name of
    Right prf -> prf
    Left ex -> error (displayException ex)

---- Closed-form least-squares linear fitting ---------------------------------

-- | Return a least-squares linear fit obtained from the closed-form least
--   squares equations.
lsqFit :: [Pt] -> Line
lsqFit [] = error "cannot least-squares fit an empty list"
lsqFit [_] = error "cannot least-squares fit a single point"
lsqFit pts =
  let n :: Int
      n = length pts

      nf :: Float
      nf = fromIntegral n

      xs, ys :: [Float]
      xs = fmap pt_x pts
      ys = fmap pt_y pts

      square :: Float -> Float
      square x = x * x

      s_x, s_y, s_xx, s_xy :: Float
      s_x = sum xs
      s_y = sum ys
      s_xx = sum $ square <$> xs
      s_xy = sum $ (\(Pt x y) -> x * y) <$> pts

      theta_1' = (nf * s_xy - s_x * s_y) / (nf * s_xx - s_x * s_x)
      theta_0' = (1 / nf) * s_y - theta_1' * (1 / nf) * s_x
   in Line theta_0' theta_1'

---- Generation of example points for linear fitting --------------------------

-- | A default (finite) set of points used for the linear fitting example.
defaultLinFitPts :: [Pt]
defaultLinFitPts =
  let seed = 42
      gen = mkStdGen seed
      line = Line 5.0 1.5
      n_points = 30
      x_range = (-5.0, 5.0)
      y_stdev = 0.75
   in take n_points (linFitPts gen line x_range y_stdev)

-- | Generate points with pseudo-random noise for a linear fit.
--
-- x values are distributed uniformly. y values follow the provided linear
-- relationship, with additional Normally-distributed noise.
linFitPts ::
  forall g.
  (RandomGen g) =>
  -- | Random generator.
  g ->
  -- | Underlying linear relationship for the generated points.
  Line ->
  -- | '(min, max)' range for the uniformly-distributed x values.
  (Float, Float) ->
  -- | Standard Deviation of the noise added to y values.
  Float ->
  -- | Infinite list of generated points.
  [Pt]
linFitPts gen line x_range stdev =
  let -- Configure random number generators.
      gen_x, gen_y :: g
      (gen_x, gen_y) = split gen

      -- Generate values.
      xs = uniforms gen_x x_range
      ys = zipWith (+) (map (linear line) xs) (normals gen_y (0.0, stdev))
   in zipWith Pt xs ys

---- PRNG sequence generation -------------------------------------------------

-- | Generate an infinite list of uniformly-distributed PRNG values.
uniforms ::
  forall a g.
  (Ord a, Num a, Random a, RandomGen g) =>
  -- | Random generator.
  g ->
  -- | '(min, max)' of the uniform distribution.
  (a, a) ->
  -- | Infinite list of generated values.
  [a]
uniforms gen (range_min, range_max) = assert (range_min <= range_max) vals
  where
    vals :: [a]
    vals = map xform (randoms gen)

    xform :: a -> a
    xform x = x * (range_max - range_min) + range_min

-- | Generate an infinite list of Normally-distributed PRNG values.
--
-- Under the hood, this generates a sequence of 'Double' values, but converts
-- them to type 'a'.
normals ::
  forall a g.
  (RealFrac a, Random a, RandomGen g) =>
  -- | Random generator.
  g ->
  -- | '(mean, stdev)' of the Normal distribution.
  (a, a) ->
  -- | Infinite list of generated values.
  [a]
normals gen (mu, stdev) =
  if stdev > 0
    then map (quantile_rf normal_dist) (randoms gen)
    else repeat 0
  where
    normal_dist :: NormalDistribution
    normal_dist = normalDistr (realToFrac mu) (realToFrac stdev)

    quantile_rf :: (ContDistr d) => d -> a -> a
    quantile_rf dist x = realToFrac (quantile dist (realToFrac x))

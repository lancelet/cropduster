{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module LinearFit
  ( -- * Types
    Pt (Pt, pt_x, pt_y),
    Line (Line, theta_0, theta_1),

    -- * Functions
    fit,
    defaultLinFitPts,
    plotPts,
    plotLineAndPts,
    linearFittingPointsAnimation,
  )
where

import Control.Exception (assert)
import Control.Monad (forM_)
import Data.Bifunctor (second)
import Data.VectorSpace (AdditiveGroup (zeroV), Scalar, VectorSpace, (*^), (^+^), (^-^), (^/))
import GHC.Generics (Generic)
import Graphics.Matplotlib ((%))
import qualified Graphics.Matplotlib as Plt
import Path (Abs, Dir, Path, (</>))
import qualified Path
import Statistics.Distribution (ContDistr, quantile)
import Statistics.Distribution.Normal (NormalDistribution, normalDistr)
import System.Random (Random, RandomGen, StdGen, mkStdGen, randoms, split)
import Text.Printf (printf)

-- | 2D Point.
data Pt = Pt
  { pt_x :: Float,
    pt_y :: Float
  }

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

data LineGrad = LineGrad
  { dLossdTheta_0 :: Float,
    dLossdTheta_1 :: Float
  }

-- | Given parameters of a linear relationship, apply the equation for the
--   line to an input.
linear :: Line -> Float -> Float
linear (Line c m) x = m * x + c

---- Linear fitting with SGD --------------------------------------------------

-- | Perform an SGD learning update for each of the supplied points.
fit ::
  -- | Learning rate.
  Float ->
  -- | Initial parameters of the line.
  Line ->
  -- | Points on which to perform learning updates. One point at a time is
  --   used to update the line parameters.
  [Pt] ->
  -- | List of linear parameters and losses produced during each step of
  --   fitting.
  [(Line, Float)]
fit gamma line [] = []
fit gamma line (pt : pts) =
  let (line', loss) = step gamma line pt
   in (line', loss) : fit gamma line' pts

-- | A single learning step.
--
-- Use the error obtained from estimating the point's y-coordinate to update
-- the parameters of the line using stochastic gradient descent.
step ::
  -- | Learning rate.
  Float ->
  -- | Current parameters of the line.
  Line ->
  -- | Point to use for this step (ground truth).
  Pt ->
  -- | Updated line parameters, and the (previous) loss for this point.
  (Line, Float)
step gamma line (Pt x y) =
  let (loss, line') = learningStep gamma linfitLossFn [Example x y] line
   in (line', loss)

---- Generic SGD --------------------------------------------------------------

-- | Mark a type as being a gradient.
newtype Grad p = Grad p deriving (Generic, AdditiveGroup, VectorSpace)

-- | Perform a gradient descent update of a value.
sgdUpdateG ::
  (VectorSpace t) =>
  -- | Learning rate.
  Scalar t ->
  -- | Initial value.
  t ->
  -- | Gradient of the value.
  Grad t ->
  -- | New value, after gradient descent update.
  t
sgdUpdateG r theta (Grad dtheta) = theta ^-^ (r *^ dtheta)

-- | Example for supervised training.
data Example i o = Example
  { example_input :: i,
    example_output :: o
  }

-- | Batch of paired inputs and outputs for supervised training.
type Batch i o = [Example i o]

-- | Loss function.
--
-- A loss function takes a batch of examples and returns a tuple containing
-- the value of the loss and the gradient of loss with respect to all the
-- parameters.
type LossFn i o t = Batch i o -> t -> (Float, Grad t)

-- | Perform a learning step of a batch of examples, updating parameters to
--   their new values and returning the loss and new parameters.
learningStep ::
  forall i o t.
  (VectorSpace t) =>
  -- | Learning rate.
  Scalar t ->
  -- | Loss function.
  LossFn i o t ->
  -- | Batch of examples.
  Batch i o ->
  -- | Current parameter(s).
  t ->
  -- | Loss and updated parameter(s).
  (Float, t)
learningStep r lf batch t = second (sgdUpdateG r t) (lf batch t)

---- Generic Linear Fit -------------------------------------------------------

-- | Loss function for a linear fit.
linfitLossFn :: LossFn Float Float Line
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
    go n sum [] = sum ^/ fromIntegral n
    go n sum (x : xs) = go (n + 1) (sum ^+^ x) xs

---- Plotting -----------------------------------------------------------------

-- | TODO: Docs.
plotLineAndPts :: Path b t -> Line -> [Pt] -> IO ()
plotLineAndPts outfile line pts = do
  let xs, ys :: [Float]
      xs = map pt_x pts
      ys = map pt_y pts
      x_min = minimum xs
      x_max = maximum xs

      scatter_plt :: Plt.Matplotlib
      scatter_plt = Plt.scatter xs ys

      line_plt :: Plt.Matplotlib
      line_plt = Plt.line [x_min, x_max] [linear line x_min, linear line x_max]

      plot :: Plt.Matplotlib
      plot = scatter_plt Plt.% line_plt

  result <- Plt.file (Path.toFilePath outfile) plot
  -- TODO: Handle error

  pure ()

-- | TODO: Docs.
plotPts :: [Pt] -> IO ()
plotPts pts = Plt.onscreen $ Plt.scatter xs ys
  where
    xs = map pt_x pts
    ys = map pt_y pts

---- Animated sequence generation ---------------------------------------------

linearFittingPointsAnimation ::
  Path Abs Dir ->
  IO ()
linearFittingPointsAnimation out_dir = do
  let training_pts = concat (replicate 6 defaultLinFitPts)
      line = Line 10 -1.2
      gamma = 2e-2

      train_result :: [(Line, Float)]
      train_result = fit gamma line training_pts

      lines :: [Line]
      lines = map fst train_result

  forM_ (zip [0 ..] lines) $ \(index :: Int, line) -> do
    filename <- Path.parseRelFile (printf "%0*d.png" (4 :: Int) index)
    let outfile = out_dir </> filename
        xs = map pt_x training_pts
        ys = map pt_y training_pts
        x_min = -6 :: Float
        x_max = 6 :: Float
        plot =
          Plt.scatter xs ys
            % Plt.line [x_min, x_max] [linear line x_min, linear line x_max]
            % Plt.xlim @Double @Double (realToFrac x_min) (realToFrac x_max)
            % Plt.ylim @Double @Double -4 13
    putStrLn $ "Rendering file: " <> Path.toFilePath filename
    Plt.file (Path.toFilePath outfile) plot

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
normals gen (mean, stdev) =
  if stdev > 0
    then map (quantile_rf normal_dist) (randoms gen)
    else repeat 0
  where
    normal_dist :: NormalDistribution
    normal_dist = normalDistr (realToFrac mean) (realToFrac stdev)

    quantile_rf :: (ContDistr d) => d -> a -> a
    quantile_rf dist x = realToFrac (quantile dist (realToFrac x))

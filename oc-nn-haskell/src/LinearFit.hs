{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
  deriving (Show)

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
step gamma line gt = (sgdUpdate line gamma (lossGrad line gt), loss line gt)

-- | Loss function for fitting a single ground truth point.
--
-- This returns the squared difference between the predicted y-value of the
-- provided point and its actual y-value.
loss ::
  -- | Current parameters of the line.
  Line ->
  -- | Point from the ground-truth dataset we're trying to fit.
  Pt ->
  -- | Square of the error between actual y-value and predicted y-value.
  Float
loss line gt =
  let Pt x_gt y_gt = gt
      -- y_pred is the predicted y value given the current line
      y_pred = linear line x_gt
      -- err_y is the difference between the ground-truth y and predicted y
      err_y = y_gt - y_pred
   in -- the returned value is the difference squared
      err_y * err_y

-- | Compute the gradient of 'loss' with respect to the line fit parameters at
--   a single ground truth point.
lossGrad ::
  -- | Current parameters of the line.
  Line ->
  -- | Point from the ground-truth dataset we're trying to fit.
  Pt ->
  -- | Gradient of the loss wrt each line parameter.
  LineGrad
lossGrad line gt =
  let Pt x_gt y_gt = gt
      y_pred = linear line x_gt
      dLossdTheta_0 = 2 * (y_pred - y_gt)
      dLossdTheta_1 = 2 * (y_pred - y_gt) * x_gt
   in LineGrad dLossdTheta_0 dLossdTheta_1

-- | Perform a stochastic gradient descent update of the line parameters.
--
-- See: https://en.wikipedia.org/wiki/Gradient_descent
-- For a description of the overall approach.
sgdUpdate ::
  -- | Current parameters of the line.
  Line ->
  -- | Learning rate.
  Float ->
  -- | Gradient of loss with respect to the line parameters.
  LineGrad ->
  -- | New parameters of the line.
  Line
sgdUpdate (Line c m) gamma (LineGrad dc dm) = Line c' m'
  where
    c' = c - gamma * dc
    m' = m - gamma * dm

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

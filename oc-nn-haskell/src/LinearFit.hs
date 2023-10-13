{-# LANGUAGE ScopedTypeVariables #-}

module LinearFit
  ( -- * Types
    Pt (Pt, pt_x, pt_y),

    -- * Functions
    defaultLinFitPts,
  )
where

import Control.Exception (assert)
import Statistics.Distribution (ContDistr, quantile)
import Statistics.Distribution.Normal (NormalDistribution, normalDistr)
import System.Random (Random, RandomGen, StdGen, mkStdGen, randoms, split)

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

-- | Given parameters of a linear relationship, apply the equation for the
--   line to an input.
linear :: Line -> Float -> Float
linear (Line c m) x = m * x + c

---- Linear fitting with SGD --------------------------------------------------

-- | Loss function for fitting a single point.
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

---- Generation of example points for linear fitting --------------------------

-- | A default (finite) set of points used for the linear fitting example.
defaultLinFitPts :: [Pt]
defaultLinFitPts =
  let seed = 42
      gen = mkStdGen seed
      line = Line 5.0 1.5
      n_points = 30
      x_range = (0.0, 10.0)
      y_stdev = 2.5
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
normals gen (mean, stdev) = map (quantile_rf normal_dist) (randoms gen)
  where
    normal_dist :: NormalDistribution
    normal_dist = normalDistr (realToFrac mean) (realToFrac stdev)

    quantile_rf :: (ContDistr d) => d -> a -> a
    quantile_rf dist x = realToFrac (quantile dist (realToFrac x))

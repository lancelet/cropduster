{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Duster.Loss
  ( -- * Functions
    listMean,
    listMeanSq,
    smootherStep,
    smootherStepWeightedTrajectoryLoss,
  )
where

import Data.Functor ((<&>))
import Data.VectorSpace (Scalar, VectorSpace, zeroV, (*^), (^+^))
import Numeric.Backprop
  ( BVar,
    Backprop,
    Reifies,
    W,
    constVar,
    sequenceVar,
    pattern T2,
  )

-- | Compute the mean of a list.
--
-- This uses the naive post-summation division approach. It is appropriate
-- only for relatively small lists.
listMean :: forall a s. (VectorSpace a, s ~ Scalar a, Fractional s) => [a] -> a
listMean [] = error "Cannot compute mean of an empty list."
listMean xs = go zeroV 0 xs
  where
    go :: a -> Int -> [a] -> a
    go run_sum count [] = (1 / fromIntegral count) *^ run_sum
    go run_sum count (x : xs_rem) = go (run_sum ^+^ x) (count + 1) xs_rem

-- | Compute the mean of the squares of all elements of a list.
listMeanSq ::
  forall a s.
  (Num a, VectorSpace a, s ~ Scalar a, Fractional s) =>
  [a] ->
  a
listMeanSq = listMean . fmap (\x -> x * x)

-- | Sum a loss over all time steps in a trajectory, additionally weighted
--   according to the 'smootherstep' function.
smootherStepWeightedTrajectoryLoss ::
  forall s p.
  (Backprop p, Reifies s W) =>
  -- | Minimum edges of the smoother step function.
  Double ->
  -- | Maximum edge of the smoother step function.
  Double ->
  -- | Compute the loss contribution of a single time sample.
  (BVar s (Double, p) -> BVar s Double) ->
  -- | Trajectory of the system over time.
  BVar s [(Double, p)] ->
  -- | Net loss
  BVar s Double
smootherStepWeightedTrajectoryLoss edge_min edge_max sample_loss_fn trajectory =
  let states :: [BVar s (Double, p)]
      states = sequenceVar trajectory

      unweighted_losses :: [BVar s Double]
      unweighted_losses = sample_loss_fn <$> states

      times :: [BVar s Double]
      times = bvarFst <$> states

      weights :: [BVar s Double]
      weights = smootherStep (constVar edge_min) (constVar edge_max) <$> times

      weighted_losses :: [BVar s Double]
      weighted_losses = zip unweighted_losses weights <&> uncurry (*)
   in sum weighted_losses

-- | 'fst' lifted to work on BVars.
bvarFst ::
  (Backprop a, Backprop b, Reifies s W) =>
  BVar s (a, b) ->
  BVar s a
bvarFst (T2 x _) = x
bvarFst _ = error "This case should not occur."

-- | Ken Perlin's "smootherstep" function.
smootherStep ::
  forall a.
  (Ord a, Fractional a) =>
  -- | Min "edge" of the function.
  a ->
  -- | Max "edge" of the function.
  a ->
  -- | Input value.
  a ->
  -- | Output value.
  a
smootherStep step_min step_max x = smootherStep01 x'
  where
    x' = (x - step_min) / (step_max - step_min)

-- | Ken Perlin's "smootherstep" function, with 'x E [0, 1]'.
smootherStep01 :: forall a. (Ord a, Num a) => a -> a
smootherStep01 x
  | x <= 0 = 0
  | x >= 1 = 1
  | otherwise = 6 * x ^ (5 :: Int) - 15 * x ^ (4 :: Int) + 10 * x ^ (3 :: Int)

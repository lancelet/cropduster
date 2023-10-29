{-# LANGUAGE ScopedTypeVariables #-}

module Duster.PRNG
  ( -- * Functions
    uniforms,
    normals,
    uniformMat,
    normalMat,
    initLinearWeight,
  )
where

import Control.Exception (assert)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeNats (KnownNat, natVal)
import Numeric.LinearAlgebra.Static (L)
import qualified Numeric.LinearAlgebra.Static as LA
import Statistics.Distribution (ContDistr, quantile)
import Statistics.Distribution.Normal (NormalDistribution, normalDistr)
import System.Random (Random, RandomGen, randoms)

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

-- | Generate a matrix whose elements are all drawn from the same uniform
--   distribution.
uniformMat ::
  forall g n m.
  (RandomGen g, KnownNat n, KnownNat m) =>
  -- | Random generator.
  g ->
  -- | '(min, max)' of the Uniform distribution.
  (Double, Double) ->
  -- | Generated matrix.
  L n m
uniformMat gen minmax = LA.fromList $ take sz $ uniforms gen minmax
  where
    sz = n * m
    n = fromIntegral $ natVal (Proxy :: Proxy n)
    m = fromIntegral $ natVal (Proxy :: Proxy m)

-- | Generate a matrix whose elements are all drawn from the same normal
--   distribution.
normalMat ::
  forall g n m.
  (RandomGen g, KnownNat n, KnownNat m) =>
  -- | Random generator.
  g ->
  -- | '(mean, stdev)' of the Normal distribution.
  (Double, Double) ->
  -- | Generated matrix.
  L n m
normalMat gen msd = LA.fromList $ take sz $ normals gen msd
  where
    sz = n * m
    n = fromIntegral $ natVal (Proxy :: Proxy n)
    m = fromIntegral $ natVal (Proxy :: Proxy m)

---- Neural network initializers ----------------------------------------------

-- | Initialize weights for a linear layer.
--
-- Weights are dran from a uniform distribution 'U(-k, k)', where
-- 'k = sqrt(6 / (n + m))'. This corresponds to "Glorot uniform" initialization
-- used by Julia for 'Dense' layers.

{-
initLinearWeight ::
  forall g n m.
  (RandomGen g, KnownNat n, KnownNat m) =>
  -- | Random generator.
  g ->
  -- | Generated matrix.
  L n m
initLinearWeight gen =
  let m, n :: Int
      m = fromIntegral $ natVal (Proxy :: Proxy m)
      n = fromIntegral $ natVal (Proxy :: Proxy n)

      gain :: Double
      gain = 2
      k = gain * sqrt(6 / realToFrac(n + m))
   in uniformMat gen (-k, k)
-}

initLinearWeight ::
  forall g n m.
  (RandomGen g, KnownNat n, KnownNat m) =>
  -- | Random generator.
  g ->
  -- | Generated matrix.
  L n m
initLinearWeight gen =
  let m :: Int
      m = fromIntegral $ natVal (Proxy :: Proxy m)

      k :: Double
      k = 1.0 / fromIntegral m

      sqrtk :: Double
      sqrtk = sqrt k
   in uniformMat gen (-sqrtk, sqrtk)

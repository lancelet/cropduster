{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Duster.Learning
  ( -- * Classes
    AdamParam (elemWiseSquare, elemWiseSqrt, elemWiseDiv),

    -- * Types
    Grad (Grad),
    AdamBufs (AdamBufs, adamBufsMoment1, adamBufsMoment2, adamBufsNSteps),
    AdamConfig
      ( AdamConfig,
        adamConfigBeta1,
        adamConfigBeta2,
        adamConfigLR,
        adamConfigEpsilon
      ),

    -- * Functions
    sgdUpdate,
    zeroAdamBufs,
    defaultAdamConfig,
    adamUpdate,
    decayWeights,
  )
where

import Data.Proxy (Proxy)
import Data.VectorSpace
  ( AdditiveGroup,
    Scalar,
    VectorSpace,
    zeroV,
    (*^),
    (^+^),
    (^-^),
  )
import Data.Void (Void)
import Duster.Orphans ()
import GHC.Generics (Generic, (:*:) ((:*:)))
import qualified GHC.Generics as G
import GHC.TypeLits (KnownNat)
import Numeric.LinearAlgebra.Static (L, R, konst)

---- SGD ----------------------------------------------------------------------

-- | Mark a type as being a gradient.
newtype Grad t = Grad t
  deriving
    ( Num,
      Fractional,
      Floating,
      Generic,
      AdditiveGroup,
      VectorSpace,
      Show
    )

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

---- ADAM ---------------------------------------------------------------------

type VRep v = G.Rep v Void

-- | AdamParam is a class for parameters that can participate in Adam
--   learning updates.
class (VectorSpace p) => AdamParam p where
  -- | Square the value element-wise (ie. square each scalar component).
  elemWiseSquare :: p -> p
  default elemWiseSquare ::
    (Generic p, AdamParam (VRep p)) =>
    p ->
    p
  elemWiseSquare p = G.to (elemWiseSquare (G.from p :: VRep p))

  -- | Take the square root element-wise (ie. take the square root of each
  --   scalar component).
  elemWiseSqrt :: p -> p
  default elemWiseSqrt ::
    (Generic p, AdamParam (VRep p)) =>
    p ->
    p
  elemWiseSqrt p = G.to (elemWiseSqrt (G.from p :: VRep p))

  -- | Perform element-wise division.
  elemWiseDiv :: p -> p -> p
  default elemWiseDiv ::
    (Generic p, AdamParam (VRep p)) =>
    p ->
    p ->
    p
  elemWiseDiv p q = G.to (elemWiseDiv (G.from p :: VRep p) (G.from q :: VRep p))

  -- | Add a scalar to all elements.
  scalarAdd :: Scalar p -> p -> p
  default scalarAdd ::
    (Generic p, AdamParam (VRep p), Scalar (VRep p) ~ Scalar p) =>
    Scalar p ->
    p ->
    p
  scalarAdd c p = G.to (scalarAdd c (G.from p :: VRep p))

instance AdamParam Double where
  elemWiseSquare :: Double -> Double
  elemWiseSqrt :: Double -> Double
  elemWiseDiv :: Double -> Double -> Double
  scalarAdd :: Scalar Double -> Double -> Double
  elemWiseSquare x = x ^ (2 :: Int)
  elemWiseSqrt = sqrt
  elemWiseDiv = (/)
  scalarAdd = (+)

instance AdamParam Float where
  elemWiseSquare :: Float -> Float
  elemWiseSqrt :: Float -> Float
  elemWiseDiv :: Float -> Float -> Float
  scalarAdd :: Scalar Float -> Float -> Float
  elemWiseSquare x = x ^ (2 :: Int)
  elemWiseSqrt = sqrt
  elemWiseDiv = (/)
  scalarAdd = (+)

instance (KnownNat n) => AdamParam (R n) where
  elemWiseSquare :: (KnownNat n) => R n -> R n
  elemWiseSqrt :: (KnownNat n) => R n -> R n
  elemWiseDiv :: (KnownNat n) => R n -> R n -> R n
  scalarAdd :: (KnownNat n) => Scalar (R n) -> R n -> R n
  elemWiseSquare x = x ^ (2 :: Int)
  elemWiseSqrt = sqrt
  elemWiseDiv x y = x / y
  scalarAdd c x = x + konst c

instance (KnownNat n, KnownNat m) => AdamParam (L n m) where
  elemWiseSquare :: (KnownNat n, KnownNat m) => L n m -> L n m
  elemWiseSqrt :: (KnownNat n, KnownNat m) => L n m -> L n m
  elemWiseDiv :: (KnownNat n, KnownNat m) => L n m -> L n m -> L n m
  scalarAdd :: (KnownNat n, KnownNat m) => Scalar (L n m) -> L n m -> L n m
  elemWiseSquare x = x ^ (2 :: Int)
  elemWiseSqrt = sqrt
  elemWiseDiv x y = x / y
  scalarAdd c x = x + konst c

instance (AdamParam a) => AdamParam (G.Rec0 a s) where
  elemWiseSquare :: (AdamParam a) => G.Rec0 a s -> G.Rec0 a s
  elemWiseSqrt :: (AdamParam a) => G.Rec0 a s -> G.Rec0 a s
  elemWiseDiv :: (AdamParam a) => G.Rec0 a s -> G.Rec0 a s -> G.Rec0 a s
  scalarAdd :: (AdamParam a) => Scalar (G.Rec0 a s) -> G.Rec0 a s -> G.Rec0 a s
  elemWiseSquare (G.K1 p) = G.K1 (elemWiseSquare p)
  elemWiseSqrt (G.K1 p) = G.K1 (elemWiseSqrt p)
  elemWiseDiv (G.K1 x) (G.K1 y) = G.K1 (elemWiseDiv x y)
  scalarAdd c (G.K1 x) = G.K1 (scalarAdd c x)

instance (AdamParam (f p)) => AdamParam (G.M1 i c f p) where
  elemWiseSquare :: (AdamParam (f p)) => G.M1 i c f p -> G.M1 i c f p
  elemWiseSqrt :: (AdamParam (f p)) => G.M1 i c f p -> G.M1 i c f p
  elemWiseDiv :: (AdamParam (f p)) => G.M1 i c f p -> G.M1 i c f p -> G.M1 i c f p
  scalarAdd ::
    (AdamParam (f p)) =>
    Scalar (G.M1 i c f p) ->
    G.M1 i c f p ->
    G.M1 i c f p
  elemWiseSquare (G.M1 p) = G.M1 (elemWiseSquare p)
  elemWiseSqrt (G.M1 p) = G.M1 (elemWiseSqrt p)
  elemWiseDiv (G.M1 x) (G.M1 y) = G.M1 (elemWiseDiv x y)
  scalarAdd c (G.M1 x) = G.M1 (scalarAdd c x)

instance
  ( AdamParam (f p),
    AdamParam (g p),
    Scalar (f p) ~ Scalar (g p)
  ) =>
  AdamParam ((f :*: g) p)
  where
  elemWiseSquare ::
    (AdamParam (f p), AdamParam (g p), Scalar (f p) ~ Scalar (g p)) =>
    (:*:) f g p ->
    (:*:) f g p
  elemWiseSqrt ::
    (AdamParam (f p), AdamParam (g p), Scalar (f p) ~ Scalar (g p)) =>
    (:*:) f g p ->
    (:*:) f g p
  elemWiseDiv ::
    (AdamParam (f p), AdamParam (g p), Scalar (f p) ~ Scalar (g p)) =>
    (:*:) f g p ->
    (:*:) f g p ->
    (:*:) f g p
  scalarAdd ::
    (AdamParam (f p), AdamParam (g p), Scalar (f p) ~ Scalar (g p)) =>
    Scalar ((:*:) f g p) ->
    (:*:) f g p ->
    (:*:) f g p
  elemWiseSquare (x :*: y) = elemWiseSquare x :*: elemWiseSquare y
  elemWiseSqrt (x :*: y) = elemWiseSqrt x :*: elemWiseSqrt y
  elemWiseDiv (x1 :*: y1) (x2 :*: y2) =
    elemWiseDiv x1 x2 :*: elemWiseDiv y1 y2
  scalarAdd c (x :*: y) = scalarAdd c x :*: scalarAdd c y

-- | Buffers for Adam learning.
data AdamBufs p = AdamBufs
  { adamBufsMoment1 :: p,
    adamBufsMoment2 :: p,
    adamBufsNSteps :: Int
  }
  deriving (Show)

-- | Zero / initial buffers for Adam.
zeroAdamBufs :: (AdamParam p) => AdamBufs p
zeroAdamBufs = let z = zeroV in AdamBufs z z 0

-- | Configuration settings for an Adam learning update.
data AdamConfig a = AdamConfig
  { adamConfigBeta1 :: a,
    adamConfigBeta2 :: a,
    adamConfigLR :: a,
    adamConfigEpsilon :: a
  }

-- | Default Adam settings.
defaultAdamConfig ::
  forall s p.
  (AdamParam p, s ~ Scalar p, Fractional s) =>
  Proxy p ->
  AdamConfig s
defaultAdamConfig _ = AdamConfig 0.9 0.999 0.001 1e-8

-- | Perform an Adam learning update.
--
-- This is the "slower but clearer" version of Adam in Algorithm 1 of:
--   - Kingma, D.P. and Ba, J.L. (2015) ADAM: A Method for Stochastic
--     Optimization. ICLR.
adamUpdate ::
  (AdamParam p, s ~ Scalar p, Floating s) =>
  -- | Configuration; containing moment parameters, learning rate and epsilon.
  AdamConfig s ->
  -- | Adam buffers for the current optimization.
  AdamBufs p ->
  -- | Gradient at the current step.
  Grad p ->
  -- | Parameters to update.
  p ->
  -- | Updated parameters and new Adam buffers.
  (p, AdamBufs p)
adamUpdate config adamBufs (Grad grad) param =
  let beta1 = adamConfigBeta1 config
      beta2 = adamConfigBeta2 config
      lr = adamConfigLR config
      eps = adamConfigEpsilon config

      m = adamBufsMoment1 adamBufs
      v = adamBufsMoment2 adamBufs
      t = 1 + adamBufsNSteps adamBufs

      m' = beta1 *^ m ^+^ (1 - beta1) *^ grad
      v' = beta2 *^ v ^+^ (1 - beta2) *^ elemWiseSquare grad
      m'' = (1 / (1 - (beta1 ^ t))) *^ m'
      v'' = (1 / (1 - (beta2 ^ t))) *^ v'
      p' = param ^-^ lr *^ elemWiseDiv m'' (scalarAdd eps (elemWiseSqrt v''))

      adamBufs' = AdamBufs m' v' t
   in (p', adamBufs')

---- Weight decay normalization -----------------------------------------------

-- | Perform weight decay.
--
-- Weights are decayed by '(1-amount)' each iteration.
decayWeights :: (VectorSpace p, s ~ Scalar p, Num s) => s -> p -> p
decayWeights amount params = (1 - amount) *^ params

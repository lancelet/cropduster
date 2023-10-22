{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Duster.Orphans where

import qualified Data.VectorSpace as VS
import GHC.TypeLits (KnownNat)
import qualified Numeric.Backprop as B
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Static as LAS

---- Compatibility between hmatrix and vector-spaces --------------------------

-- | AdditiveGroup for statically-sized vectors.
instance VS.AdditiveGroup (LAS.R n) where
  zeroV :: LAS.R n
  zeroV = 0

  (^+^) :: LAS.R n -> LAS.R n -> LAS.R n
  (^+^) = (+)

  negateV :: LAS.R n -> LAS.R n
  negateV = negate

  (^-^) :: LAS.R n -> LAS.R n -> LAS.R n
  (^-^) = (-)

-- | AdditiveGroup for statically-sized matrices.
instance (KnownNat n, KnownNat m) => VS.AdditiveGroup (LAS.L n m) where
  zeroV :: (KnownNat n, KnownNat m) => LAS.L n m
  zeroV = 0

  (^+^) :: (KnownNat n, KnownNat m) => LAS.L n m -> LAS.L n m -> LAS.L n m
  (^+^) = (+)

  negateV :: (KnownNat n, KnownNat m) => LAS.L n m -> LAS.L n m
  negateV = negate

  (^-^) :: (KnownNat n, KnownNat m) => LAS.L n m -> LAS.L n m -> LAS.L n m
  (^-^) = (-)

-- VectorSpace for statically-sized vectors.
instance (KnownNat n) => VS.VectorSpace (LAS.R n) where
  type Scalar (LAS.R n) = LAS.ℝ

  (*^) :: (KnownNat n) => VS.Scalar (LAS.R n) -> LAS.R n -> LAS.R n
  c *^ x = LAS.konst c * x

-- VectorSpace for statically-sized matrices.
instance (KnownNat n, KnownNat m) => VS.VectorSpace (LAS.L n m) where
  type Scalar (LAS.L n m) = LAS.ℝ

  (*^) ::
    (KnownNat n, KnownNat m) =>
    VS.Scalar (LAS.L n m) ->
    LAS.L n m ->
    LAS.L n m
  c *^ x = LAS.konst c * x

-- InnerSpace for statically-sized vectors.
instance (KnownNat n) => VS.InnerSpace (LAS.R n) where
  (<.>) :: (KnownNat n) => LAS.R n -> LAS.R n -> VS.Scalar (LAS.R n)
  (<.>) = LAS.dot

-- InnerSpace for statically-sized matrices.
--
-- This uses an element-wise dot product. This is for compatibility with the
-- backward pass for operations that require a vector-Jacobian product sum.
instance (KnownNat n, KnownNat m) => VS.InnerSpace (LAS.L n m) where
  (<.>) ::
    (KnownNat n, KnownNat m) =>
    LAS.L n m ->
    LAS.L n m ->
    VS.Scalar (LAS.L n m)
  x <.> y = LA.sumElements $ LAS.extract (x * y)

---- Compatibility between backprop and vector-space --------------------------

-- | AdditiveGroup for BVar.
instance
  ( B.Backprop a,
    B.Reifies s B.W,
    VS.AdditiveGroup a
  ) =>
  VS.AdditiveGroup (B.BVar s a)
  where
  zeroV :: (B.Backprop a, B.Reifies s B.W, VS.AdditiveGroup a) => B.BVar s a
  zeroV = B.auto VS.zeroV
  (^+^) ::
    (B.Backprop a, B.Reifies s B.W, VS.AdditiveGroup a) =>
    B.BVar s a ->
    B.BVar s a ->
    B.BVar s a
  (^+^) = B.liftOp2 $ B.op2 opAddV
  negateV ::
    (B.Backprop a, B.Reifies s B.W, VS.AdditiveGroup a) =>
    B.BVar s a ->
    B.BVar s a
  negateV = B.liftOp1 $ B.op1 opNegateV
  (^-^) ::
    (B.Backprop a, B.Reifies s B.W, VS.AdditiveGroup a) =>
    B.BVar s a ->
    B.BVar s a ->
    B.BVar s a
  (^-^) = B.liftOp2 $ B.op2 opSubV

-- | Operation to add two vectors elementwise, including backward pass.
opAddV ::
  (VS.AdditiveGroup a) =>
  -- | First value to be added.
  a ->
  -- | Second value to be added.
  a ->
  -- | Result of addition and gradient function.
  (a, a -> (a, a))
opAddV x y = ((VS.^+^) x y, \dl -> (dl, dl))

-- | Operation to negate a vector elementwise, including backward pass.
opNegateV ::
  (VS.AdditiveGroup a) =>
  -- | Value to be negated.
  a ->
  -- | Result of negation and gradient function.
  (a, a -> a)
opNegateV x = (x, VS.negateV)

-- | Operation to subtract two vectors elementwise, including backward pass.
opSubV ::
  (VS.AdditiveGroup a) =>
  -- | Value to be subtracted from.
  a ->
  -- | Value to subtract.
  a ->
  -- | Result of subtraction and gradient function.
  (a, a -> (a, a))
opSubV x y = ((VS.^-^) x y, \dl -> (dl, VS.negateV dl))

-- | VectorSpace for BVar.
instance
  ( B.Backprop a,
    B.Reifies s B.W,
    c ~ VS.Scalar a,
    B.Backprop c,
    VS.VectorSpace a,
    VS.InnerSpace a
  ) =>
  VS.VectorSpace (B.BVar s a)
  where
  type Scalar (B.BVar s a) = B.BVar s (VS.Scalar a)
  (*^) ::
    ( B.Backprop a,
      B.Reifies s B.W,
      c ~ VS.Scalar a,
      B.Backprop c,
      VS.VectorSpace a,
      VS.InnerSpace a
    ) =>
    VS.Scalar (B.BVar s a) ->
    B.BVar s a ->
    B.BVar s a
  (*^) = B.liftOp2 $ B.op2 opScalarMulV

-- | Operation to multiply a scalar by a vector, including backward pass.
opScalarMulV ::
  ( VS.VectorSpace a,
    VS.InnerSpace a,
    c ~ VS.Scalar a
  ) =>
  -- | Scalar value.
  c ->
  -- | Vector value.
  a ->
  -- | Result of multiplication, and gradient function.
  (a, a -> (c, a))
opScalarMulV k x = (k VS.*^ x, \dl -> (dl VS.<.> x, k VS.*^ dl))
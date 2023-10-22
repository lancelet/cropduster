{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MassSpringDamper where

import Data.VectorSpace
  ( AdditiveGroup (negateV, zeroV, (^+^), (^-^)),
    InnerSpace,
    VectorSpace (Scalar, (*^)),
    (<.>),
  )
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat)
import qualified Graphics.Matplotlib as Plt
import Lens.Micro.TH (makeLenses)
import Numeric.Backprop
  ( BVar,
    Backprop,
    Reifies,
    W,
    auto,
    collectVar,
    evalBP,
    liftOp1,
    liftOp2,
    op1,
    op2,
    (^^.),
    pattern T2,
  )
import Numeric.LinearAlgebra ((!))
import Numeric.LinearAlgebra.Static (Sized (extract), dot, fromList, konst)
import Numeric.LinearAlgebra.Static.Backprop
  ( L,
    R,
    matrix,
    (#>),
  )
import ODE (Grad (Grad), odeIntRK4, termt)

type State = R 2

---- AdditiveGroup, VectorSpace and InnerSpace for R n ------------------------

instance AdditiveGroup (R n) where
  zeroV = 0
  (^+^) = (+)
  negateV = negate
  (^-^) = (-)

instance (KnownNat n) => VectorSpace (R n) where
  type Scalar (R n) = Double
  c *^ x = konst c * x

instance (KnownNat n) => InnerSpace (R n) where
  (<.>) = dot

---- AdditiveGroup and VectorSpace for BVar s a -------------------------------

instance
  (AdditiveGroup a, Backprop a, Reifies s W) =>
  AdditiveGroup (BVar s a)
  where
  zeroV = auto zeroV
  (^+^) = liftOp2 (op2 elementWiseAdd)
    where
      elementWiseAdd :: a -> a -> (a, a -> (a, a))
      elementWiseAdd x y = (x ^+^ y, \dl -> (dl, dl))
  negateV = liftOp1 (op1 elementWiseNegation)
    where
      elementWiseNegation :: a -> (a, a -> a)
      elementWiseNegation x = (x, negateV)
  (^-^) = liftOp2 (op2 elementWiseSub)
    where
      elementWiseSub :: a -> a -> (a, a -> (a, a))
      elementWiseSub x y = (x ^-^ y, \dl -> (dl, negateV dl))

instance
  ( VectorSpace a,
    InnerSpace a,
    Backprop a,
    Backprop (Scalar a),
    Reifies s W
  ) =>
  VectorSpace (BVar s a)
  where
  type Scalar (BVar s a) = BVar s (Scalar a)
  (*^) = liftOp2 (op2 scalarMul)
    where
      scalarMul :: Scalar a -> a -> (a, a -> (Scalar a, a))
      scalarMul s x = (s *^ x, \dl -> (dl <.> x, s *^ dl))

data Param = Param
  { _param_m :: Double,
    _param_k :: Double,
    _param_c :: Double
  }
  deriving (Generic, Show)

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

integ :: IO ()
integ = do
  let s0 :: R 2
      s0 = fromList [1, 0]

      param :: BVar s Param
      param = auto $ Param 1 50 2

      stop_fn = termt (auto 5)

      h :: BVar s Double
      h = auto 0.02

      states ::
        forall s.
        (Reifies s W) =>
        (BVar s Double, BVar s (R 2)) ->
        [(BVar s Double, BVar s (R 2))]
      states = odeIntRK4 h (gradFn param) stop_fn

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
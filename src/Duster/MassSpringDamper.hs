{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Duster.MassSpringDamper where

import Duster.ODE (Grad (Grad), odeIntRK4, termt)
import Duster.Orphans ()
import GHC.Generics (Generic)
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
    (^^.),
    pattern T2,
  )
import Numeric.LinearAlgebra ((!))
import Numeric.LinearAlgebra.Static (Sized (extract), fromList)
import Numeric.LinearAlgebra.Static.Backprop
  ( L,
    R,
    matrix,
    (#>),
  )

type State = R 2

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

integ1 :: IO ()
integ1 = do
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
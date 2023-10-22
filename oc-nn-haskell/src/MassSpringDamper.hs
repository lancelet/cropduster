{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module MassSpringDamper where

import Data.VectorSpace
  ( AdditiveGroup (negateV, zeroV, (^+^), (^-^)),
    VectorSpace (Scalar, (*^)),
  )
import qualified Graphics.Matplotlib as Plt
import Numeric.LinearAlgebra ((!))
import Numeric.LinearAlgebra.Static
  ( L,
    R,
    Sized (extract),
    fromList,
    konst,
    (#>),
  )
import ODE (Grad (Grad), odeIntRK4, termt)

newtype State = State {unState :: R 2} deriving (Show)

instance AdditiveGroup State where
  zeroV = State $ konst 0
  State x ^+^ State y = State $ x + y
  negateV (State x) = State $ -x
  State x ^-^ State y = State $ x - y

instance VectorSpace State where
  type Scalar State = Double
  c *^ x = State $ unState x * konst c

data Param = Param
  { param_m :: Double,
    param_k :: Double,
    param_c :: Double
  }

gradFn :: Param -> Double -> State -> Grad State
gradFn ps _t (State x) = Grad . State $ mm #> x
  where
    m = param_m ps
    k = param_k ps
    c = param_c ps

    mm :: L 2 2
    mm = fromList [0, 1, -k / m, -c / m]

integ :: IO ()
integ = do
  let s0 = State $ fromList [1, 0]
      param = Param 1 50 2
      grad_fn = gradFn param
      stop_fn = termt 5
      h = 0.02
      states = odeIntRK4 h grad_fn stop_fn (0, s0)

      ts :: [Double]
      ts = fst <$> states

      xs :: [Double]
      xs = flip (!) 0 . extract . unState . snd <$> states

      plot = Plt.plot ts xs

  Plt.onscreen plot
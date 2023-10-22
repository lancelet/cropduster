{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Duster.ODE
  ( -- * Types
    Grad (Grad, unGrad),

    -- * Functions
    odeIntRK4,
    termt
  )
where

import Data.List (unfoldr)
import Data.VectorSpace (AdditiveGroup, Scalar, VectorSpace, (*^), (^+^))
import GHC.Generics (Generic)

newtype Grad t = Grad {unGrad :: t}
  deriving (Generic, AdditiveGroup, VectorSpace)

-- | Integrate an ODE using a 4th-order Runge Kutta method until a termination
--   function indicates that it has completed.
odeIntRK4 ::
  forall s p.
  (VectorSpace p, s ~ Scalar p, Fractional s) =>
  -- | Step size (fixed).
  s ->
  -- | Function that returns the gradient (ie. the ODE to integrate).
  (s -> p -> Grad p) ->
  -- | Function that determines when integration should terminate. When this
  --   function returns `True` then the integration will terminate.
  (s -> p -> Bool) ->
  -- | Initial state.
  (s, p) ->
  -- | Computed time evolution of states.
  [(s, p)]
odeIntRK4 h grad_fn stop_fn init_val =
  let unfold_fn :: (s, p) -> Maybe ((s, p), (s, p))
      unfold_fn (t, y) =
        if stop_fn t y
          then Nothing
          else let !o = rk4Step h grad_fn (t, y) in Just (o, o)
   in init_val : unfoldr unfold_fn init_val

-- | Single step of a 4th-order Runge-Kutta integration.
rk4Step ::
  forall s p.
  (VectorSpace p, s ~ Scalar p, Fractional s) =>
  -- | Size of time step.
  s ->
  -- | Function that returns the gradient (ie. the ODE to integrate).
  (s -> p -> Grad p) ->
  -- | Initial state.
  (s, p) ->
  -- | Final state.
  (s, p)
rk4Step h grad_fn (t, y) =
  let g s p = unGrad (grad_fn s p)
      h2 = h / 2
      h6 = h / 6

      k1 = g t y
      k2 = g (t + h2) (y ^+^ h2 *^ k1)
      k3 = g (t + h2) (y ^+^ h2 *^ k2)
      k4 = g (t + h) (y ^+^ h *^ k3)

      t' = t + h
      y' = y ^+^ h6 *^ (k1 ^+^ (2 *^ k2) ^+^ (2 *^ k3) ^+^ k4)
   in (t', y')

-- | Function to terminate integration at a fixed time, when `t >= tf`.
termt :: (Ord s) => s -> (s -> p -> Bool)
termt tf t _y = t >= tf
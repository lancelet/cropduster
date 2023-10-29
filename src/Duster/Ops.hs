{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Duster.Ops
  ( -- * Functions
    vecToScalar,
    vecToTuple4,
    vecToTuple5,
    scalarToVec,
    tuple3ToVec,
    tuple4ToVec,
    tuple5ToVec,
    vecToList,
    listToVec,
    minimumBy,
  )
where

import Data.AdditiveGroup (AdditiveGroup (zeroV))
import GHC.TypeLits (KnownNat)
import Numeric.Backprop (BVar, Backprop, Op, Reifies, W, collectVar, liftOp1, op1, sequenceVar)
import qualified Numeric.LinearAlgebra as LA (toList)
import Numeric.LinearAlgebra.Static (fromList, unwrap)
import Numeric.LinearAlgebra.Static.Backprop (R)

vecToScalar ::
  (Reifies s W) =>
  BVar s (R 1) ->
  BVar s Double
vecToScalar v =
  case sequenceVar . vecToList $ v of
    [x] -> x
    _ -> error "This case should not occur - vector size is type-checked."

-- | Convert a 4-element vector to a tuple containing its 4 elements.
vecToTuple4 ::
  (Reifies s W) =>
  BVar s (R 4) ->
  (BVar s Double, BVar s Double, BVar s Double, BVar s Double)
vecToTuple4 v =
  case sequenceVar . vecToList $ v of
    [x1, x2, x3, x4] -> (x1, x2, x3, x4)
    _ -> error "This case should not occur - vector size is type-checked."

-- | Convert a 5-element vector to a tuple containing its 5 elements.
vecToTuple5 ::
  (Reifies s W) =>
  BVar s (R 5) ->
  (BVar s Double, BVar s Double, BVar s Double, BVar s Double, BVar s Double)
vecToTuple5 v =
  case sequenceVar . vecToList $ v of
    [x1, x2, x3, x4, x5] -> (x1, x2, x3, x4, x5)
    _ -> error "This case should not occur - vector size is type-checked."

scalarToVec ::
  (Reifies s W) =>
  BVar s Double ->
  BVar s (R 1)
scalarToVec x = listToVec . collectVar $ [x]

tuple3ToVec ::
  (Reifies s W) =>
  (BVar s Double, BVar s Double, BVar s Double) ->
  BVar s (R 3)
tuple3ToVec (x1, x2, x3) = listToVec . collectVar $ [x1, x2, x3]

tuple4ToVec ::
  (Reifies s W) =>
  (BVar s Double, BVar s Double, BVar s Double, BVar s Double) ->
  BVar s (R 4)
tuple4ToVec (x1, x2, x3, x4) = listToVec . collectVar $ [x1, x2, x3, x4]

tuple5ToVec ::
  (Reifies s W) =>
  (BVar s Double, BVar s Double, BVar s Double, BVar s Double, BVar s Double) ->
  BVar s (R 5)
tuple5ToVec (x1, x2, x3, x4, x5) = listToVec . collectVar $ [x1, x2, x3, x4, x5]

-- | Convert an 'R n' vector to a list, lifted to 'BVar's.
vecToList :: (KnownNat n, Reifies s W) => BVar s (R n) -> BVar s [Double]
vecToList = liftOp1 vecToListOp

-- | Operation that converts an 'R n' vector to a list.
--
-- The backward pass of this operation converts the list of gradients back to
-- an 'R n' vector.
vecToListOp :: (KnownNat n) => Op '[R n] [Double]
vecToListOp = op1 $ \v -> (LA.toList . unwrap $ v, fromList)

-- | Convert a list of 'Double's to an 'R n' vector.
listToVec :: (KnownNat n, Reifies s W) => BVar s [Double] -> BVar s (R n)
listToVec = liftOp1 listToVecOp

-- | Operation that converts a list of 'Double's to an 'R n' vector.
--
-- The backward pass of this operation converts the 'R.n' vector back to a
-- list of 'Double's.
listToVecOp :: (KnownNat n) => Op '[[Double]] (R n)
listToVecOp = op1 $ \xs -> (fromList xs, LA.toList . unwrap)

minimumBy ::
  (AdditiveGroup a, Ord b, Eq a, Backprop a, Reifies s W) =>
  (a -> b) ->
  BVar s [a] ->
  BVar s a
minimumBy f = liftOp1 $ minimumByOp f

minimumByOp ::
  forall a b.
  (Eq a, AdditiveGroup a, Ord b) =>
  (a -> b) ->
  Op '[[a]] a
minimumByOp f = op1 $ \xs ->
  let (minItem, minIndex) = minWithIndex f xs
      nxs = length xs
      back grad =
        let grad_list =
              concat
                [ replicate (minIndex - 1) zeroV,
                  [grad],
                  replicate (nxs - minIndex - 1) zeroV
                ]
         in grad_list
   in (minItem, back)

minWithIndex :: forall a b. (Ord b) => (a -> b) -> [a] -> (a, Int)
minWithIndex _ [] = error "Cannot find the minimum of an empty list."
minWithIndex _ [x] = (x, 0)
minWithIndex f (x : xs) = go 0 x (f x) 0 xs
  where
    go :: Int -> a -> b -> Int -> [a] -> (a, Int)
    go min_index min_item _ _ [] = (min_item, min_index)
    go min_index min_item min_val cur_index (x' : xs') =
      let trial_val = f x'
       in if trial_val < min_val
            then go cur_index x' trial_val (cur_index + 1) xs'
            else go min_index min_item min_val (cur_index + 1) xs'

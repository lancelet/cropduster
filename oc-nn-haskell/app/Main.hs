{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.List (intercalate)
import LinearFit
  ( Line (Line),
    Pt (Pt),
    defaultLinFitPts,
    fit,
    plotLineAndPts,
    plotPts,
    linearFittingPointsAnimation
  )
import qualified Path

main :: IO ()
main = do
  let pts :: [Pt]
      pts = defaultLinFitPts

      training_pts :: [Pt]
      training_pts = concat [pts, pts, pts]

      line = Line 0 0
      gamma = 1e-3
      train_result :: [(Line, Float)]
      train_result = fit gamma line training_pts

      strs :: [String]
      strs = map (\(Pt x y) -> "(" <> show x <> ", " <> show y <> ")") pts

      str :: String
      str = intercalate "\n" strs

  linearFittingPointsAnimation [Path.reldir|plots|]
  putStrLn str
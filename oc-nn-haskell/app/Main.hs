module Main where

import Data.List (intercalate)
import LinearFit (Pt (Pt), Line(Line), defaultLinFitPts, fit)

main :: IO ()
main = do
  let pts :: [Pt]
      pts = defaultLinFitPts

      ptSeq :: [Pt]
      ptSeq = concat [pts, pts, pts]

      line = Line 0 0
      gamma = 0.005
      losses :: [Float]
      losses = map snd (fit gamma line ptSeq)

      strs :: [String]
      strs = map (\(Pt x y) -> "(" <> show x <> ", " <> show y <> ")") pts

      str :: String
      str = intercalate "\n" strs

  putStrLn str
  print losses
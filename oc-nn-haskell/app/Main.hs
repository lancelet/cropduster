module Main where

import Data.List (intercalate)
import LinearFit (Pt (Pt), defaultLinFitPts)

main :: IO ()
main = do
  let pts :: [Pt]
      pts = defaultLinFitPts

      strs :: [String]
      strs = map (\(Pt x y) -> "(" <> show x <> ", " <> show y <> ")") pts

      str :: String
      str = intercalate "\n" strs

  putStrLn str
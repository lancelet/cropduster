module Main where

import Data.List (intercalate)

import LinearFit (Pt(Pt), linfitPts)

main :: IO ()
main = do
    let
        pts :: [Pt]
        pts = take 30 linfitPts
        
        strs :: [String]
        strs = map (\(Pt x y) -> "(" <> show x <> ", " <> show y <> ")") pts
        
        str :: String
        str = intercalate "\n" strs

    putStrLn str
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.List (intercalate)
import LinearFit
  ( Line (Line),
    Pt (Pt),
    defaultLinFitPts,
    fit,
    linearFittingPointsAnimation,
    plotLineAndPts,
    plotPts,
  )
import Path (reldir, (</>), Path, Abs, Dir)
import qualified Path
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)

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

  curDir <- getCurrentDirectory >>= Path.parseAbsDir

  let plotDir = curDir </> [reldir|plots|]
  ensureDir plotDir
  linearFittingPointsAnimation plotDir

  putStrLn str

-- | Ensure a directory exists, creating all necessary directories.
ensureDir :: Path Abs Dir -> IO ()
ensureDir dir = createDirectoryIfMissing True (Path.toFilePath dir)
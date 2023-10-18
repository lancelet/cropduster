{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.List (intercalate)
import LinearFit
  ( Batch,
    Example (Example),
    Line (Line),
    Pt (Pt),
    defaultLinFitPts,
    fit,
    linearFittingPointsAnimation,
    linfitLossFn,
  )
import Path (Abs, Dir, Path, reldir, (</>))
import qualified Path
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)

main :: IO ()
main = do
  let pts :: [Pt]
      pts = defaultLinFitPts

      training_pts :: [Pt]
      training_pts = concat [pts, pts, pts]

      line = Line 0 0
      gamma = 1e-2
      train_result :: [(Float, Line)]
      train_result = fit gamma linfitLossFn batches line
        where
          batches :: [Batch Float Float]
          batches = fmap (\(Pt x y) -> [Example x y]) training_pts

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
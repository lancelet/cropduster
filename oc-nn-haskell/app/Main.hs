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
    linearFittingAnimation,
    linfitLossFn,
  )
import Path (Abs, Dir, Path, reldir, (</>))
import qualified Path
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)

main :: IO ()
main = do
  curDir <- getCurrentDirectory >>= Path.parseAbsDir
  let plotDir = curDir </> [reldir|plots|]
  ensureDir plotDir
  linearFittingAnimation plotDir 4 2e-2

-- | Ensure a directory exists, creating all necessary directories.
ensureDir :: Path Abs Dir -> IO ()
ensureDir dir = createDirectoryIfMissing True (Path.toFilePath dir)
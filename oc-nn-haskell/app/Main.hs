{-# LANGUAGE QuasiQuotes #-}

module Main where

import LinearFit (linearFittingAnimation, lossLandscapeAnimation)
import Path (Abs, Dir, Path, reldir, (</>))
import qualified Path
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)

main :: IO ()
main = do
  curDir <- getCurrentDirectory >>= Path.parseAbsDir
  let plotDir = curDir </> [reldir|plots|]

  {-
  -- Plot linear fits with 1 batch element
  let linFitBS1 = plotDir </> [reldir|linfit-bs1|]
  ensureDir linFitBS1
  linearFittingAnimation linFitBS1 120 1 2e-2

  -- Plot linear fits with 4 batch element
  let linFitBS4 = plotDir </> [reldir|linfit-bs4|]
  ensureDir linFitBS4
  linearFittingAnimation linFitBS4 120 4 2e-2
  -}

  let lldir = plotDir </> [reldir|loss-landscape|]
  ensureDir lldir
  lossLandscapeAnimation lldir 4 2e-2

-- | Ensure a directory exists, creating all necessary directories.
ensureDir :: Path Abs Dir -> IO ()
ensureDir dir = createDirectoryIfMissing True (Path.toFilePath dir)
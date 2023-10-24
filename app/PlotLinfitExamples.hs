{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Duster.LinearFit (linearFittingAnimation, lossLandscapeAnimation)
import Duster.Log (createLogger)
import Options.Applicative (Parser)
import qualified Options.Applicative as OA
import Path (Abs, Dir, Path, reldir, (</>))
import Path.IO (ensureDir, resolveDir')

-- | Main entry point for file.
main :: IO ()
main = generatePlots =<< OA.execParser opts
  where
    opts =
      OA.info
        (parseArgs OA.<**> OA.helper)
        ( OA.fullDesc
            <> OA.progDesc "Generate linear fitting example plots"
        )

-- | Entry point for app - generate plots.
generatePlots :: Args -> IO ()
generatePlots args = do
  -- Thread-safe logger. We want just one instance of this because it locks
  -- to write a message.
  logger <- createLogger

  -- Directory to output plots
  plotDir :: Path Abs Dir <- resolveDir' (plots_dir args)

  -- Plot linear fit (batch size of 1)
  let dir_linfit_bs1 = plotDir </> [reldir|linfit-bs1|]
  ensureDir dir_linfit_bs1
  linearFittingAnimation logger dir_linfit_bs1 120 1 2e-2

  -- Plot loss landscape (batch size of 4)
  let dir_loss_landscape = plotDir </> [reldir|loss-landscape|]
  ensureDir dir_loss_landscape
  lossLandscapeAnimation dir_loss_landscape 4 2e-2

---- Option parsing -----------------------------------------------------------

-- | Command-line arguments.
newtype Args = Args
  { -- | Directory to output plots.
    plots_dir :: String
  }

-- | Parse command line arguments.
parseArgs :: Parser Args
parseArgs =
  Args
    <$> OA.strOption
      ( OA.long "dir"
          <> OA.metavar "OUT_DIR"
          <> OA.help "Output directory"
      )

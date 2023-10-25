{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Duster.FFMpeg (Framerate (Framerate), encodeProjectMovies)
import Duster.LinearFit (linearFittingAnimation, lossLandscapeAnimation)
import Duster.Log (createLogger)
import Options.Applicative (Parser)
import qualified Options.Applicative as OA
import Path (Abs, Dir, Path, relfile, (</>))
import qualified Path.IO

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
  plotDir :: Path Abs Dir <- Path.IO.resolveDir' (plots_dir args)
  Path.IO.ensureDir plotDir

  -- Plot linear fit (batch size of 1)
  let linfit_bs1_file = plotDir </> [relfile|linfit-bs1|]
  Path.IO.withSystemTempDir "linfit_bs1" $ \images_dir -> do
    linearFittingAnimation logger images_dir 120 1 2e-2
    encodeProjectMovies (Framerate 4) images_dir linfit_bs1_file

  -- Plot loss landscape animation (batch size of 4)
  let loss_landscape_file = plotDir </> [relfile|loss-landscape|]
  Path.IO.withSystemTempDir "loss_landscape" $ \images_dir -> do
    lossLandscapeAnimation logger images_dir 4 2e-2
    encodeProjectMovies (Framerate 4) images_dir loss_landscape_file

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
          <> OA.help "Movie file output directory"
      )

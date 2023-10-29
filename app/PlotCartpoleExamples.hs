{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Duster.CartPole (renderAnimation)
import Duster.FFMpeg (Framerate (Framerate), encodeProjectMovies)
import Duster.Log (createLogger)
import Options.Applicative (Parser)
import qualified Options.Applicative as OA
import Path (Abs, Dir, Path, relfile, (</>))
import qualified Path.IO

main :: IO ()
main = generatePlots =<< OA.execParser opts
  where
    opts =
      OA.info
        (parseArgs OA.<**> OA.helper)
        ( OA.fullDesc
            <> OA.progDesc "Generate cartpole example plots"
        )

generatePlots :: Args -> IO ()
generatePlots args = do
  -- Thread-safe logger
  logger <- createLogger

  -- Directory to output plots
  plotDir :: Path Abs Dir <- Path.IO.resolveDir' (plots_dir args)
  Path.IO.ensureDir plotDir

  -- Plot progress of cartpole controller
  let cartpole_learning_file = plotDir </> [relfile|cartpole_learning|]
  Path.IO.withSystemTempDir "cartpole_learning" $ \images_dir -> do
    renderAnimation logger images_dir
    encodeProjectMovies (Framerate 30) images_dir cartpole_learning_file

---- Argument parsing ---------------------------------------------------------

-- | Command line arguments.
newtype Args = Args
  { -- | Directory to output plots.
    plots_dir :: String
  }

-- | Parse command-lie arguments.
parseArgs :: Parser Args
parseArgs =
  Args
    <$> OA.strOption
      (OA.long "dir" <> OA.metavar "OUT_DIR" <> OA.help "Output directory")

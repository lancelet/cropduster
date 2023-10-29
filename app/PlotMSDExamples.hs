{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Duster.MassSpringDamper (integ2)
import Options.Applicative (Parser)
import qualified Options.Applicative as OA
import Path (Abs, Dir, Path, reldir, (</>))
import Path.IO (ensureDir, resolveDir')

main :: IO ()
main = generatePlots =<< OA.execParser opts
  where
    opts =
      OA.info
        (parseArgs OA.<**> OA.helper)
        ( OA.fullDesc
            <> OA.progDesc "Generate mass-spring-damper fitting example plots"
        )

generatePlots :: Args -> IO ()
generatePlots args = do
  -- DIrectory to output plots
  plotDir :: Path Abs Dir <- resolveDir' (plots_dir args)

  -- Plot fitting of mass-spring-damper.
  let dir_msd_fit = plotDir </> [reldir|msd-fit|]
  ensureDir dir_msd_fit
  integ2 dir_msd_fit

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

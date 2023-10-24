module Duster.Plot
  ( -- * Functions
    savePngPlot',
    savePngPlot,
  )
where

import qualified Codec.Picture.Png as Png (encodePng)
import qualified Data.ByteString.Lazy as LBS (ByteString, writeFile)
import qualified Diagrams as D (mkWidth, renderDia)
import qualified Diagrams.Backend.Rasterific as BR
  ( Options (RasterificOptions),
    Rasterific (Rasterific),
  )
import qualified Graphics.Rendering.Chart as C (vectorAlignmentFns)
import qualified Graphics.Rendering.Chart.Backend.Diagrams as CBD
  ( DEnv,
    defaultEnv,
    runBackendR,
  )
import qualified Graphics.Rendering.Chart.Renderable as CR (Renderable)
import Path (Abs, File, Path, toFilePath)

-- | Save a renderable plot as a PNG file at 640x480.
savePngPlot' ::
  -- | Renderable chart.
  CR.Renderable a ->
  -- | Path to the PNG file.
  Path Abs File ->
  -- | IO action.
  IO ()
savePngPlot' = savePngPlot 640 480 1.5

-- | Save a renderable plot as a PNG file.
savePngPlot ::
  -- | Width in pixels of the output image.
  Int ->
  -- | Height in pixels of the output image.
  Int ->
  -- | Scale factor for text, etc. A larger scale factor makes the plot
  --   "larger" inside the available space in the image.
  Double ->
  -- | Renderable chart.
  CR.Renderable a ->
  -- | Path to the PNG file.
  Path Abs File ->
  -- | IO action.
  IO ()
savePngPlot width height scale renderable out_file = do
  let sw, sh :: Double
      sw = fromIntegral width / scale
      sh = fromIntegral height / scale
  env <- CBD.defaultEnv C.vectorAlignmentFns sw sh
  LBS.writeFile
    (toFilePath out_file)
    (renderableToPngByteString env width renderable)

-- | Convert a renderable chart into a ByteString containing a PNG file.
renderableToPngByteString ::
  -- | Environment.
  CBD.DEnv Double ->
  -- | Width in pixels.
  Int ->
  -- | Renderable chart.
  CR.Renderable a ->
  -- | Lazy bytestring.
  LBS.ByteString
renderableToPngByteString env width renderable =
  let sz = D.mkWidth (fromIntegral width)
   in Png.encodePng $
        D.renderDia BR.Rasterific (BR.RasterificOptions sz) $
          fst $
            CBD.runBackendR env renderable

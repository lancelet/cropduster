{-# LANGUAGE ScopedTypeVariables #-}

-- | ffmpeg invocation via a separate process.
--
-- This module is intended to encode numbered files from a directory into
-- the formats required by the project.
--
-- Originally, this was being done using bash scripts, but they become
-- unwieldy quickly when chanes are required.
module Duster.FFMpeg
  ( -- * Types
    Framerate (Framerate),

    -- * Functions
    encodeProjectMovies,
  )
where

import Codec.Picture (dynamicMap, imageHeight, imageWidth, readImage)
import Data.Colour (Colour)
import Data.Colour.Names (white)
import Data.Colour.SRGB (RGB (RGB), toSRGB24)
import Data.Word (Word8)
import Numeric (showHex)
import Path (Abs, Dir, File, Path, addExtension)
import qualified Path
import qualified Path.IO
import System.Process (callProcess)

-- | Encode a movies in multiple formats for the project.
--
-- Currently, these are:
--   - webm with a transparent background
--   - mp4 with a white background
encodeProjectMovies ::
  -- | Framerate for this movie.
  Framerate ->
  -- | Directory containing the numbered images. The images must all be
  --   numbered and have a number field 6 digits wide.
  Path Abs Dir ->
  -- | Base name of the output file. This will have extensions added to it
  --   for the various file types.
  Path Abs File ->
  -- | IO action to create the movies.
  IO ()
encodeProjectMovies fr images_dir out_file_base = do
  webm <- addExtension ".webm" out_file_base
  mp4 <- addExtension ".mp4" out_file_base
  encode fr VP9 VORBIS Nothing images_dir webm
  encode fr X264 AAC (Just white) images_dir mp4

-- | Frame rate; frames per second.
newtype Framerate = Framerate Int

-- | Video codec.
data VCodec
  = -- | VP9 codec for webm.
    VP9
  | -- | H.264 codec for mp4.
    X264

-- | Audio codec.
data ACodec
  = -- | AAC codec for mp4.
    AAC
  | -- | Vorbis codec for web.
    VORBIS

-- | Encode a video by launching an ffmpeg process.
encode ::
  -- | Framerate - frames per second.
  Framerate ->
  -- | Video codec to use.
  VCodec ->
  -- | Audio codec to use.
  ACodec ->
  -- | Optional background color to composite behind PNG files.
  Maybe (Colour Double) ->
  -- | Directory containing numbered images. These must be 6-digits wide.
  Path Abs Dir ->
  -- | Output file name.
  Path Abs File ->
  -- | IO action to perform the encoding.
  IO ()
encode fr vcodec acodec maybe_color images_dir out_file = do
  case maybe_color of
    Nothing -> encodeWithoutOverlay fr vcodec acodec images_dir out_file
    Just c -> encodeWithOverlay fr vcodec acodec c images_dir out_file

-- | Encode a video that does not require overlaying PNG files on a
--   background color.
encodeWithoutOverlay ::
  Framerate ->
  VCodec ->
  ACodec ->
  Path Abs Dir ->
  Path Abs File ->
  IO ()
encodeWithoutOverlay fr vcodec acodec images_dir out_file = do
  let args :: [String] =
        concatMap
          unCP
          [ CP ["-y"],
            framerateToCP fr,
            CP ["-i", Path.toFilePath images_dir <> "%06d.png"],
            vcodecToCP vcodec,
            acodecToCP acodec,
            CP ["-b:v", "1M"], -- video bitrate
            CP [Path.toFilePath out_file]
          ]
  callProcess "ffmpeg" args

-- | Encode a video that does require overlaying PNG files on a background
--   color.
--
-- This is a bit more complicated because we have to find a file in the
-- directory and figure out its size before the command to create a
-- background plate can be established.
encodeWithOverlay ::
  Framerate ->
  VCodec ->
  ACodec ->
  Colour Double ->
  Path Abs Dir ->
  Path Abs File ->
  IO ()
encodeWithOverlay fr vcodec acodec bg_color images_dir out_file = do
  -- \| Find the size of one image in the images directory.
  (w, h) <- findFileSize images_dir
  let args :: [String] =
        concatMap
          unCP
          [ CP ["-y"],
            CP
              [ "-f",
                "lavfi",
                "-i",
                "color=c="
                  <> colourToHex bg_color
                  <> ":s="
                  <> show w
                  <> "x"
                  <> show h
              ],
            framerateToCP fr,
            CP ["-i", Path.toFilePath images_dir <> "%06d.png"],
            CP ["-shortest"],
            CP
              [ "-filter_complex",
                "[0:v][1:v]overlay=shortest=1,format=yuv420p[out]"
              ],
            CP ["-map", "[out]"],
            vcodecToCP vcodec,
            acodecToCP acodec,
            CP [Path.toFilePath out_file]
          ]
  callProcess "ffmpeg" args

-- | Command-line parameter.
newtype CP = CP {unCP :: [String]}

-- | Convert framerate to command-line parameter.
framerateToCP :: Framerate -> CP
framerateToCP (Framerate fr) = CP ["-framerate", show fr]

-- | Convert vcodec to command-line parameter.
vcodecToCP :: VCodec -> CP
vcodecToCP VP9 = CP ["-vcodec", "libvpx-vp9"]
vcodecToCP X264 = CP ["-vcodec", "libx264"]

-- | Convert ACodec to a command line parameter.
acodecToCP :: ACodec -> CP
acodecToCP AAC = CP ["-acodec", "aac"]
acodecToCP VORBIS = CP ["-acodec", "libvorbis"]

-- | Find the size of one file in the images directory.
findFileSize :: Path Abs Dir -> IO (Int, Int)
findFileSize images_dir = do
  (_, files) <- Path.IO.listDir images_dir
  let file :: Path Abs File = head files
  maybe_image <- readImage (Path.toFilePath file)
  case maybe_image of
    Left _ ->
      error $
        "JuicyPixels could not find size of image: " <> Path.toFilePath file
    Right img -> pure (dynamicMap imageWidth img, dynamicMap imageHeight img)

-- | Convert a colour to an RGB hex string.
colourToHex :: Colour Double -> String
colourToHex colour =
  let RGB r g b = toSRGB24 colour
   in "#" <> componentToHex r <> componentToHex g <> componentToHex b
  where
    componentToHex :: Word8 -> String
    componentToHex x =
      let h = showHex x ""
       in if length h == 1 then "0" <> h else h

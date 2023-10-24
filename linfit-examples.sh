#!/usr/bin/env bash
#
# This bash script generates the linear fitting examples and converts them to
# movies using ffmpeg.

FRAMERATE=4

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PLOT_DIR="${SCRIPT_DIR}/plots"
HUGO_STATIC_DIR="${SCRIPT_DIR}/gh-site/static/video"

# Generate all examples
cabal run linfit-examples -- --dir="$PLOT_DIR"

# Encode linfit-bs1 movies
ffmpeg \
  -y \
  -framerate "$FRAMERATE" \
  -i "$PLOT_DIR/linfit-bs1/%04d.png" \
  -c:v libvpx-vp9 \
  -b:v 1M \
  -c:a libvorbis \
  -vf "format=yuva420p" \
  "$PLOT_DIR/linfit-bs1.webm"
ffmpeg \
  -y \
  -f lavfi -i color=c=white:s=640x480 \
  -framerate "$FRAMERATE" \
  -i "$PLOT_DIR/linfit-bs1/%04d.png" \
  -shortest \
  -filter_complex "[0:v][1:v]overlay=shortest=1,format=yuv420p[out]" \
  -map "[out]" \
  -vcodec libx264 \
  -acodec aac \
  "$PLOT_DIR/linfit-bs1.mp4"

# Encode loss-landscape.webm
ffmpeg \
  -y \
  -framerate "$FRAMERATE" \
  -i "$PLOT_DIR/loss-landscape/%04d.png" \
  -vf "format=yuv420p" \
  -c:v libvpx \
  -b:v 1M \
  -c:a libvorbis \
  "$PLOT_DIR/loss-landscape.webm"
ffmpeg \
  -y \
  -f lavfi -i color=c=white:s=640x480 \
  -framerate "$FRAMERATE" \
  -i "$PLOT_DIR/loss-landscape/%04d.png" \
  -shortest \
  -filter_complex "[0:v][1:v]overlay=shortest=1,format=yuv420p[out]" \
  -map "[out]" \
  -vcodec libx264 \
  -acodec aac \
  "$PLOT_DIR/loss-landscape.mp4"

# Copy video(s) to the Hugo site
mkdir -p "$HUGO_STATIC_DIR"
cp "$PLOT_DIR/"*.webm "$HUGO_STATIC_DIR/."
cp "$PLOT_DIR/"*.mp4 "$HUGO_STATIC_DIR/."
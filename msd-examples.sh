#!/usr/bin/env bash
#
# This bash script generates the mass-spring-damper fitting examples and
# converts them to movies using ffmpeg.

FRAMERATE=12

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PLOT_DIR="${SCRIPT_DIR}/plots"
HUGO_STATIC_DIR="${SCRIPT_DIR}/gh-site/static/video"

# Generate all examples
cabal run msd-examples -- --dir="$PLOT_DIR"

# Encode linfit-bs1.webm
ffmpeg \
  -y \
  -framerate "$FRAMERATE" \
  -i "$PLOT_DIR/msd-fit/%04d.png" \
  -vf "format=yuv420p" \
  -c:v libvpx \
  -b:v 1M \
  -c:a libvorbis \
  "$PLOT_DIR/msd-fit.webm"
ffmpeg \
  -y \
  -f lavfi -i color=c=white:s=640x480 \
  -framerate "$FRAMERATE" \
  -i "$PLOT_DIR/msd-fit/%04d.png" \
  -shortest \
  -filter_complex "[0:v][1:v]overlay=shortest=1,format=yuv420p[out]" \
  -map "[out]" \
  -vcodec libx264 \
  -acodec aac \
  "$PLOT_DIR/msd-fit.mp4"

# Copy video(s) to the Hugo site
mkdir -p "$HUGO_STATIC_DIR"
cp "$PLOT_DIR/"*.webm "$HUGO_STATIC_DIR"
cp "$PLOT_DIR/"*.mp4 "$HUGO_STATIC_DIR/."
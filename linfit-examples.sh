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

# Encode linfit-bs1.webm
ffmpeg \
  -y \
  -framerate "$FRAMERATE" \
  -i "$PLOT_DIR/linfit-bs1/%04d.png" \
  -vf "format=yuv420p" \
  -c:v libvpx \
  -b:v 1M \
  -c:a libvorbis \
  "$PLOT_DIR/linfit-bs1.webm"

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

# Copy video(s) to the Hugo site
mkdir -p "$HUGO_STATIC_DIR"
cp "$PLOT_DIR/"*.webm "$HUGO_STATIC_DIR/."
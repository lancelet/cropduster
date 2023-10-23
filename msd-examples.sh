#!/usr/bin/env bash
#
# This bash script generates the mass-spring-damper fitting examples and
# converts them to movies using ffmpeg.

FRAMERATE=12

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PLOT_DIR="${SCRIPT_DIR}/plots"

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
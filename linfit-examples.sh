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

# Copy video(s) to the Hugo site
mkdir -p "$HUGO_STATIC_DIR"
cp "$PLOT_DIR/"*.webm "$HUGO_STATIC_DIR/."
cp "$PLOT_DIR/"*.mp4 "$HUGO_STATIC_DIR/."
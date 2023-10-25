#!/usr/bin/env bash

pandoc \
  -t revealjs \
  -s \
  --mathjax \
  -o index.html \
  slides.md \
  --include-in-header=slides.css \
  -V theme=solarized

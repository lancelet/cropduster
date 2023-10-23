#!/usr/bin/env bash

pandoc \
  -t revealjs \
  -s \
  --mathjax \
  -o index.html \
  slides.md \
  --include-in-header=slides.css \
  -V revealjs-url=https://unpkg.com/reveal.js/ \
  -V theme=solarized

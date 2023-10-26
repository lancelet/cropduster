#!/usr/bin/env bash

pandoc --version
pandoc \
  -t revealjs \
  -s \
  --mathjax \
  -o index.html \
  slides.md \
  --include-in-header=slides.css \
  -V theme=solarized \
  -V revealjs-url=https://unpkg.com/reveal.js/

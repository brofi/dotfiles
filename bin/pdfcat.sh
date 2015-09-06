#!/bin/bash

EXT=.pdf

read -p "PDF 1: " PDF1
read -p "PDF 2: " PDF2

OUTPUT=${PDF1}_${PDF2}${EXT}

pdftk $PDF1$EXT $PDF2$EXT cat output $OUTPUT

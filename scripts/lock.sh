#!/bin/bash

IMAGE=/tmp/i3lock.png

#BLURTYPE="0x5" # 7.52s
#BLURTYPE="0x2" # 4.39s
#BLURTYPE="5x3" # 3.80s
BLURTYPE="2x8" # 2.90s
#BLURTYPE="2x3" # 2.92s

scrot $IMAGE
convert $IMAGE -level 0%,100%,0.6 -blur $BLURTYPE $IMAGE
i3lock -i $IMAGE
rm $IMAGE

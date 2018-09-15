#!/bin/sh -x

. ~/.screenlayout/shared.sh

xrandr --output ${INTERNAL} --mode 1920x1080 --pos 0x0 --rotate normal ${OFFS}

. ~/.screenlayout/bg.sh

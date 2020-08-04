#!/bin/sh -x

. ~/.screenlayout/shared.sh

xrandr
xrandr --output ${EXTERNAL} --mode ${EXTERNAL_MODE} --pos 0x0 --rotate normal ${OFFS}

. ~/.screenlayout/bg.sh

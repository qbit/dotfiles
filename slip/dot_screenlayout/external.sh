#!/bin/sh
pkill trayer
xrandr
xrandr --output VIRTUAL1 --off --output eDP1 --off --output DP1 --mode 2560x1440 --pos 0x0 --rotate normal --output HDMI2 --off --output HDMI1 --off --output DP2 --off
. ~/.screenlayout/bg.sh 
. ~/.screenlayout/tray.sh "external"

#!/bin/sh

pkill trayer

if [ "$1" == "external" ]; then
	trayer --edge top --align right --width 5 --height 12 --alpha 180 --tint 0 --transparent true --SetDockType true --SetPartialStrut true --expand true --widthtype percent
else
	trayer --edge top --align right --width 9 --height 12 --alpha 180 --tint 0 --transparent true --SetDockType true --SetPartialStrut true --expand true --widthtype percent
fi

#!/bin/sh

feh --bg-fill ~/.background.png
#pkill xbattbar
#xbattbar -I "#99aab9" -O "#798a99" -o "#99aab9" -i "#798a99" &
#xbattbar -i green -o "olive drab" -I blue -O red &

pkill lemonbar-xft
while true; do ~/bin/bar; sleep 5; done | lemonbar-xft -d -f "Go Mono:pixelsize=12" -B "#FFFFEA"  -F "#000000" | sh &

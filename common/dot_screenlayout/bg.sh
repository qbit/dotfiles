#!/bin/sh

#feh --bg-fill ~/.background.png
#pkill xbattbar
#xbattbar -I "#99aab9" -O "#798a99" -o "#99aab9" -i "#798a99" &
#xbattbar -i green -o "olive drab" -I blue -O red &

pkill lemonbar-xft
while true; do ~/bin/bar; sleep 5; done | lemonbar-xft -d -f "Fantasque Sans Mono:pixelsize=14" -B "#2E3440"  -F "#D8DEE9" | sh &

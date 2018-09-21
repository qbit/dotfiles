INTERNAL="eDP-1"
EXTERNAL="DP-2-1"
EXTERNAL_MODE="2560x1440"

OFFS=""
for d in $( xrandr | grep ^[a-zA-Z] | grep -v Screen | awk '{print $1}'); do
	OFFS="${OFFS} --output ${d} --off"
done

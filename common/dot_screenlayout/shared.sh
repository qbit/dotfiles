INTERNAL="eDP"
EXTERNAL="DisplayPort-1"
EXTERNAL_MODE="3840x2160"

OFFS=""
for d in $( xrandr | grep ^[a-zA-Z] | grep -v Screen | awk '{print $1}'); do
	OFFS="${OFFS} --output ${d} --off"
done

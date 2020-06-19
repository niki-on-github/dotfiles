#!/usr/bin/env sh
# Description: Switch and select Window with dmenu
# NOTE: currently only work on x11

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""


windowList=$(wmctrl -l)
windowList="${windowList//$(hostname)/}"
choice=$(echo "$windowList" | eval "dmenu -p \"GOTO Window >\" -l 10 -i $DMENU_STYLE") || exit

# ist window on i3 scratchpad?
if [ "$(echo $choice | cut -d " " -f2)" = "-1" ]; then
    exec i3-msg "[id=\"$(echo $choice | cut -d " " -f1)\"] scratchpad show"
else
    wmctrl -i -a $(echo $choice | cut -d " " -f1)
    xdotool getactivewindow mousemove 0 0 mousemove --window %1 0 0 click 1
fi

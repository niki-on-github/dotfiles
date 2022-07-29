#!/usr/bin/env sh

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""

# TODO get global for all dmenu scripts
if [ "$(xdotool get_desktop)" -gt "8" ]; then
    DMENU_STYLE="-fn 'Hack-18' -h 35"
fi

if command -v j4-dmenu-desktop >/dev/null ; then
    j4-dmenu-desktop --dmenu="dmenu -i $DMENU_STYLE"
else
    dmenu_run
fi


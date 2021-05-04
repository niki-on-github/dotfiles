#!/usr/bin/env sh

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""

if command -v j4-dmenu-desktop >/dev/null ; then
    j4-dmenu-desktop --dmenu="dmenu -i $DMENU_STYLE"
else
    dmenu_run
fi


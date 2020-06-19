#!/usr/bin/env sh

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""

j4-dmenu-desktop --dmenu="dmenu -i $DMENU_STYLE"


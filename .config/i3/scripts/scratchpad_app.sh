#!/bin/bash
# Description: This script was created to get a program from the i3 scratchpad. Additional function: If the program is not running yet, the program will be started.

# add to i3/config:
# --
# for_window [class="$1"] floating enable, move to scratchpad
# bindsym $mod+x exec --no-startup-id ~/.config/i3/scripts/scratchpad_app.sh "$1" "$2"
# --
# param:
# $1 : window class
# $2 : application


[ -z "$1" ] && exit 1
[ -z "$2" ] && exit 1
[ -z "$(command -v $2)" ] && notify-send "Error" "Command $2 not found" && exit 1

class=$1; shift
if ! i3-msg "[class=\"$class\"] scratchpad show" ; then
    eval "$@" & >/dev/null 2>&1
    sleep 1.25
    i3-msg "[class=\"$class\"] scratchpad show"
fi

exit

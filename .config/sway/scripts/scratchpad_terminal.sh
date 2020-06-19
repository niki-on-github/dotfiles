#!/bin/bash
# Description: This script was created to get a program from the sway scratchpad. Additional function: If the program is not running yet, the program will be started.

# add to sway/config:
# --
# for_window [app_id="$1"] floating enable, move to scratchpad
# bindsym $mod+x exec ~/.config/sway/scripts/scratchpad_terminal.sh "$1" "$2"
# --
# param:
# $1 : app_id
# $2 : terminal application


[ -z "$1" ] && exit 1
[ -z "$2" ] && exit 1
[ -z "$TERMINAL" ] && notify-send "Error" "Environment variable \$TERMINAL is not set!" && exit 1
[ -z "$(command -v $2)" ] && notify-send "Error" "Command $2 not found" && exit 1
[ "$TERMINAL" != "alacritty" ] && notify-send "Error" "Currently only alacritty is supported as terminal" && exit 1

app_id="$1"; shift
if ! swaymsg "[app_id=\"$app_id\"] scratchpad show" ; then
    eval "$TERMINAL --class \"$app_id\" -t \"$app_id\" -d 150 50 -e \"$@\"" & >/dev/null 2>&1
    sleep 0.8
    swaymsg "[app_id=\"$app_id\"] scratchpad show"
    swaymsg "[app_id=\"$app_id\"] move position center"
fi

exit

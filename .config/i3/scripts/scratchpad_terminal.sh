#!/bin/bash
# Description: This script was created to get a program from the i3 scratchpad. Additional function: If the program is not running yet, the program will be started.

# add to i3/config:
# --
# for_window [instance="$1"] floating enable, move to scratchpad
# bindsym $mod+x exec --no-startup-id ~/.config/i3/scripts/scratchpad_terminal.sh "$1" "$2"
# --
# param:
# $1 : instance name (assigns any unique application string without spaces)
# $2 : terminal application


[ -z "$1" ] && exit 1
[ -z "$2" ] && exit 1
[ -z "$TERMINAL" ] && notify-send "Error" "Environment variable \$TERMINAL is not set!" && exit 1
[ -z "$(command -v $2)" ] && notify-send "Error" "Command $2 not found" && exit 1
[ "$TERMINAL" != "alacritty" ] && notify-send "Error" "Currently only alacritty is supported as terminal" && exit 1

instance="$1"; shift
if ! i3-msg "[instance=\"$instance\"] scratchpad show" ; then
    eval "$TERMINAL --class \"$instance\" -t \"$instance\" -d 150 50 -e \"$@\"" & >/dev/null 2>&1
    sleep 0.8
    i3-msg "[instance=\"$instance\"] scratchpad show"
    #[old] i3-msg "[instance=\"$instance\"] resize set 1200px 800px
    i3-msg "[instance=\"$instance\"] move position center"
fi

exit

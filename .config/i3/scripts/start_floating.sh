#!/bin/bash

[ -z "$1" ] && echo "ERROR: Application variable \$1 is not set" && exit 1
[ -z "$2" ] && width=1200px || width=$2
[ -z "$3" ] && height=800px || height=$3
[ -z "$TERMINAL" ] && notify-send "Error" "Environment variable \$TERMINAL is not set!" && exit 1
[ "$TERMINAL" != "alacritty" ] && notify-send "Error" "Currently only alacritty is supported as terminal" && exit 1
[ -z "$(command -v $1)" ] && notify-send "Error" "Command $1 not found" && exit 1

$TERMINAL -t "$1" --class "$1" -e "$1" & >/dev/null 2>&1
sleep 0.25s
i3-msg "[instance=\"$1\"] floating enable"
i3-msg "[instance=\"$1\"] resize set $width $height"
i3-msg "[instance=\"$1\"] move position center"

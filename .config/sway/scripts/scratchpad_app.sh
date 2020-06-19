#!/bin/bash
# Description: This script was created to get a program from the sway scratchpad. Additional function: If the program is not running yet, the program will be started.

# add to sway/config:
# --
# for_window [class="$1"] floating enable, move to scratchpad
# bindsym $mod+x exec ~/.config/sway/scripts/scratchpad_app.sh "class" "$2" "$3"
# --
# param:
# $1 : 'app_id' or 'class'
# $2 : app_id or class (use `swaymsg -t get_tree`)
# $3 : application


[ -z "$1" ] && exit 1
[ -z "$2" ] && exit 1
[ -z "$3" ] && exit 1
[ -z "$(command -v $3)" ] && notify-send "Error" "Command $3 not found" && exit 1

Class() {
    class=$1; shift
    if ! swaymsg "[class=\"$class\"] scratchpad show" ; then
        eval "$@" & >/dev/null 2>&1
        sleep 1.25
        swaymsg "[class=\"$class\"] scratchpad show"
    fi
}

AppID() {
    app_id=$1; shift
    if ! swaymsg "[app_id=\"$app_id\"] scratchpad show" ; then
        eval "$@" & >/dev/null 2>&1
        sleep 1.25
        swaymsg "[app_id=\"$app_id\"] scratchpad show"
    fi
}

case "$1" in
    class) shift; Class "$@" ;;
    app_id) shift; AppID "$@" ;;
esac >/dev/null

#!/usr/bin/env sh

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""
[ ! -f $HOME/.local/bin/2FA ] && notify-send "Error" "FileNotFound: ~/.local/bin/2FA" && exit 1


oath_script="$HOME/.local/bin/2FA"
itemList=$($oath_script list)
[ "$?" != 0 ] && exit 1
service=$(echo "${itemList[@]}" | eval "dmenu -i -p \"2FA >\" $DMENU_STYLE") || exit
exec $oath_script code "$service"

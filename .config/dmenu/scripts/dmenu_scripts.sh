#!/usr/bin/env sh

DMENU_SCRIPTS_PATH="$HOME/.config/dmenu/scripts"
[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""


error() { notify-send "ERROR" "$@"; exit 1; }

choice=$(ls -A $DMENU_SCRIPTS_PATH | sed 's/dmenu_//g' | sed 's/.sh$//g' | eval "dmenu -i -p \"scripts >\" $DMENU_STYLE") || exit
script=$(ls -A $DMENU_SCRIPTS_PATH | grep -E "(${choice}.sh$|${choice}$)")
[ $(echo "$script" | wc -l) = 1 ] || error "Assignment of the selection is not possible"
eval "${DMENU_SCRIPTS_PATH}/$script"

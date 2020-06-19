#!/usr/bin/env sh

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""
[ -z "$TERMINAL" ] && notify-send "Error" "Environment variable \$TERMINAL is not set!" && exit 1
[ -z "$SUDO_ASKPASS" ] && notify-send "Warning" "Environment variable \$SUDO_ASKPASS is not set!"

programs=()
[ ! -z "$(command -v htop)" ] && programs+=('htop')
[ ! -z "$(command -v glances)" ] && programs+=('glances')
[ ! -z "$(command -v gtop)" ] && programs+=('gtop')
[ ! -z "$(command -v iftop)" ] && programs+=('iftop')
[ ! -z "$(command -v iotop)" ] && programs+=('iotop')


choice=$(printf '%s\n' "${programs[@]}" | sort -f | eval "dmenu -i -l 10 -p \"System Monitoring >\" $DMENU_STYLE") || exit
case $choice in
    htop|glances|gtop) exec $TERMINAL -e $choice ;;
    *|iftopp|iotop) exec sudo -A $TERMINAL -e $choice ;;
esac

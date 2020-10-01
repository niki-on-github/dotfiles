#!/usr/bin/env sh
# Description: Get a menu for lock, shutdown and reboot computer

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""

LOCK="$HOME/.local/bin/x11-lock"  # TODO wayland
[ -f $LOCK ] || notify-send "ERROR" "Locker not found: $LOCK"

menu=(' ﰸ cancel ' '  lock ' '  lockout ' '  shutdown ' '  reboot ')
choice=$(printf '%s\n' "${menu[@]}" | eval "dmenu -i -p \"shutdown menu >\" $DMENU_STYLE") || exit

case "$choice" in
    '  lock ')     # no additional query
                        eval "$LOCK" ;;
    '  lockout ')  if [ "$(printf ' cancel\n yes' |  eval "dmenu -i -p \" logout >\" $DMENU_STYLE")" == " yes" ]; then
                        grep "dwm$" <<< $DESKTOP_SESSION >/dev/null && pkill dwm && exit
                        grep "xmonad$" <<< $DESKTOP_SESSION >/dev/null && pkill xmonad && exit
                        grep "i3$" <<< $DESKTOP_SESSION >/dev/null && i3-msg exit && exit
                        grep "i3-gaps$" <<< $DESKTOP_SESSION >/dev/null && i3-msg exit && exit
                        grep "sway$" <<< $DESKTOP_SESSION >/dev/null && swaymsg exit && exit
                        notify-send "ERROR" "Lockout for your Window Manager is not implemented"
                    fi ;;
    '  shutdown ') [ "$(printf ' cancel\n yes' |  eval "dmenu -i -p \" shutdown >\" $DMENU_STYLE")" == " yes" ] \
                        && poweroff ;;
    '  reboot ')   [ "$(printf ' cancel\n yes' |  eval "dmenu -i -p \" reboot >\" $DMENU_STYLE")" == " yes" ] \
                        && reboot ;;
esac >/dev/null


#!/usr/bin/env sh
# Description: Menu for lock, logout, shutdown and reboot

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""

LOCK="$HOME/.local/bin/x11-lock"  # TODO wayland
[ -f $LOCK ] || notify-send "ERROR" "Locker not found: $LOCK"

menu=(' ﰸ cancel ' '  lock ' '  lockout ' '  shutdown ' '  reboot ')
choice=$(printf '%s\n' "${menu[@]}" | eval "dmenu -i -p \"shutdown menu >\" $DMENU_STYLE") || exit

case "$choice" in
    '  lock ')     eval "$LOCK" ;;
    '  lockout ')  if [ "$(printf ' cancel\n yes' |  eval "dmenu -i -p \" logout >\" $DMENU_STYLE")" == " yes" ]; then
                        session=$(loginctl session-status | head -n 1 | cut -d' ' -f1)
                        [ -n "$session" ] && loginctl terminate-session $session
                    fi ;;
    '  shutdown ') if [ "$(printf ' cancel\n yes' |  eval "dmenu -i -p \" shutdown >\" $DMENU_STYLE")" == " yes" ]; then
                        poweroff
                    fi ;;
    '  reboot ')   if [ "$(printf ' cancel\n yes' |  eval "dmenu -i -p \" reboot >\" $DMENU_STYLE")" == " yes" ]; then
                        reboot
                    fi ;;
esac >/dev/null


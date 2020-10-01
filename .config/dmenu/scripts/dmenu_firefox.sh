#!/bin/sh

[ ! -f $HOME/.mozilla/firefox/profiles.ini ] && echo "[ERROR] FileNotFound: $HOME/.mozilla/firefox/profiles.ini" && exit 1

choice=$(grep "^Name=" $HOME/.mozilla/firefox/profiles.ini | cut -c 6- | sort | uniq | grep -v "^default$" | dmenu -p "Firefox Profile:") || exit
[ -z "$choice" ] && exit
! grep "^Name=$choice" $HOME/.mozilla/firefox/profiles.ini >/dev/null && notify-send "Firefox" "Profile $choice not found!" && exit 1
setsid firefox -new-instance -P "$choice" >/dev/null 2>&1 &

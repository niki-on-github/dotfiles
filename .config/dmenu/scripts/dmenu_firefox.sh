#!/bin/sh

if command -v firefox ; then
    _firefox=firefox
elif command -v firefox-bin ; then
    _firefox=firefox-bin
else
    echo "firefox is not installed!"
    exit 1
fi

[ ! -f $HOME/.mozilla/firefox/profiles.ini ] && echo "[ERROR] FileNotFound: $HOME/.mozilla/firefox/profiles.ini" && exit 1

choice=$(grep "^Name=" $HOME/.mozilla/firefox/profiles.ini | cut -c 6- | sort | uniq | grep -v "^default$" | dmenu -i -p "Firefox Profile:") || exit
[ -z "$choice" ] && exit
! grep "^Name=$choice" $HOME/.mozilla/firefox/profiles.ini >/dev/null && notify-send "Firefox" "Profile $choice not found!" && exit 1
setsid $_firefox -new-instance -P "$choice" >/dev/null 2>&1 &

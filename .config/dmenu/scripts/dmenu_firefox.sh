#!/bin/sh

_vopono=$HOME/.cargo/bin/vopono

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

if echo "$choice" | grep -q -i "vpn" ; then
    if [ ! -e $_vopono ] ; then
        notify-send "ERROR" "vopono is not installed"
    else
        if [ ! -d $HOME/.config/vopono/air/openvpn ]; then
            notify-send "vopono" "first run 'vopono sync'"
            echo "vopono: first run 'vopono sync'"
            exit 1
        fi
        random_server=$(find ~/.config/vopono/air/openvpn -iname '*ovpn' -exec basename {} \; | sort -R | tail -1)
        random_server=${random_server::-5}
        notify-send "VPN" "Server: $random_server"
        $_vopono -A exec --disable-ipv6 --provider airvpn --server $random_server "firefox -no-remote -P $choice" >/tmp/vpn.log 2>&1 & disown
    fi
else
    setsid $_firefox -new-instance -P "$choice" >/dev/null 2>&1 &
fi

#!/usr/bin/env sh

killall -q polybar
interface=$(nmcli device status | grep "ethernet" | grep "verbunden" | awk '{print $1}' | head -n1)
while pgrep -u $UID -x polybar >/dev/null; do sleep 0.5; done

if [-z "$interface" ]; then
    polybar top &
else
    cp -f ~/.config/polybar/config /tmp/polybar.conf
    sed -i "s/interface = eno1/interface = $interface/g" /tmp/polybar.conf
    polybar -c /tmp/polybar.conf top &
fi

exit 0

#!/bin/sh

INTERFACE="tun*"

if [ -n "$(ifconfig | grep "^$INTERFACE")" ]; then
    if [ ! -f /tmp/vpn-ip.cache ]; then
        sleep 4
        curl -s https://ipinfo.io/ip > /tmp/vpn-ip.cache 2>&1
    fi

    ip="$(cat /tmp/vpn-ip.cache | tail -n1)"
    [ -z "$ip" ] && ip="OFF" && rm /tmp/vpn-ip.cache 
    [ "${#ip}" -gt "39" ] && ip="OFF" && rm /tmp/vpn-ip.cache 

    exec echo "$ip"
else
    [ -f /tmp/vpn-ip.cache ] && rm -f /tmp/vpn-ip.cache 
    exec echo "OFF"
fi

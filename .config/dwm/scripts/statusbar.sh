#!/bin/bash
# NOTE: use Monospace font in dwm (bar)

# execute only one script at a time
[ "$(pgrep -f dwm/scripts/autostart.sh -c)" -gt 1 ] && exit


SEP="  -  "
BAT=$(ls /sys/class/power_supply | grep "^BAT")


vol() {
    if command -v pamixer >/dev/null ; then
        VOL=$(pamixer --get-volume-human | tr -d '%')
    else
        VOL=$(amixer get Master | grep -o "[0-9]*%" | head -n 1 | tr -d '%')
    fi
    if [ "$VOL" = "muted" ] || [ "$VOL" -eq 0 ]; then
        printf " MUTE "
    else
        printf "VOL%3d%%" "$VOL"
    fi
}

mem() {
    printf "MEM%3d%%" "$(free -m | head -n2 | tail -n1 | awk '{print int(($2 - $7) * 100 / $2)}')"
}

calc_cpu() {
    CPU_U_OLD=${CPU_U-$(grep 'cpu ' /proc/stat | awk '{print $2+$4}')}
    CPU_T_OLD=${CPU_T-$(grep 'cpu ' /proc/stat | awk '{print $2+$4+$5}')}
    CPU_U=$(grep 'cpu ' /proc/stat | awk '{print $2+$4}')
    CPU_T=$(grep 'cpu ' /proc/stat | awk '{print $2+$4+$5}')
    CPU=`expr 100 \* \( $CPU_U - $CPU_U_OLD \) / \( $CPU_T - $CPU_T_OLD \)`
}

#NOTE: we use a seperate print function as workaround for bash variable scope in subshells
cpu() {
    printf "CPU%3d%%" "$CPU"
}

clock() {
    date +"%b %d, %Y${SEP}%H:%M:%S"
}

battery() {
    for battery in /sys/class/power_supply/BAT? ; do
	    capacity=$(cat "$battery"/capacity) || continue
	    status=$(sed "s/[Dd]ischarging//;s/[Nn]ot charging//;s/[Cc]harging//;s/[Uu]nknown//;s/[Ff]ull//" "$battery"/status)
        printf "BAT%3d%%%s" "$capacity" "$status" && break
    done
}

# we calc sum of all ethernet interfaces
calc_netspeed() {
    NET_R_OLD=${NET_R-$(($(cat /sys/class/net/e*/statistics/rx_bytes | paste -sd '+')))}
    NET_T_OLD=${NET_T-$(($(cat /sys/class/net/e*/statistics/tx_bytes | paste -sd '+')))}
    NET_R=$(($(cat /sys/class/net/e*/statistics/rx_bytes | paste -sd '+')))
    NET_T=$(($(cat /sys/class/net/e*/statistics/tx_bytes | paste -sd '+')))
    NET_TKBPS=`expr \( $NET_T - $NET_T_OLD \) / 1024`
    NET_RKBPS=`expr \( $NET_R - $NET_R_OLD \) / 1024`
}

#NOTE: we use a seperate print function as workaround for bash variable scope in subshells
netspeed() {
    if [ "$NET_TKBPS" -gt 1024 ]; then
        val=`expr $NET_TKBPS / 1024`
        UP="$(printf "%03dMb/s" "$val")"
    elif [ "$NET_TKBPS" -gt 999 ]; then
        UP="001Mb/s"
    else
        UP="$(printf "%03dkb/s" "$NET_TKBPS")"
    fi

    if [ "$NET_RKBPS" -gt 1024 ]; then
        val=`expr $NET_RKBPS / 1024`
        DOWN="$(printf "%03dMb/s" "$val")"
    elif [ "$NET_RKBPS" -gt 999 ]; then
        DOWN="001Mb/s"
    else
        DOWN="$(printf "%03dkb/s" "$NET_RKBPS")"
    fi

    printf "%s %s" "$UP" "$DOWN"
}

# Statusbar loop
while true; do
    # call every second to update values
    calc_cpu
    calc_netspeed

    # bar elements with 1 sec update intervall
    bar=()
    bar+=("$(cpu)" "$SEP")
    bar+=("$(mem)" "$SEP")
    [ -n "$BAT" ] && bar+=("$(battery)" "$SEP")
    bar+=("$(netspeed)" "$SEP")
    bar+=("$(clock)")  # last element has no seperator

    # update bar 3 times per second
    for i in {0..2}; do
        xsetroot -name "$(printf '%s' "$(vol)${SEP}${bar[@]}" )"
        # echo "$(printf '%s' "$(vol)${SEP}${bar[@]}" )"  # debug
        sleep 0.333
    done
done

#!/usr/bin/env sh
# Description: Gives a dmenu prompt to mount unmounted usb drives

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""
[ -z "$SUDO_ASKPASS" ] && notify-send "Warning" "Environment variable \$SUDO_ASKPASS is not set!"


mountFailed() {
    notify-send "💻 USB mounting failed" "$1 could not mounted" && exit 1
}


mountUsb() {
	chosen="$(echo "$1" | eval "dmenu -i -p \"Mount >\" $DMENU_STYLE" | awk '{print $1}')" || exit

	# mount fstab devices
	if [ ! -z "$(grep "$chosen" /etc/fstab)" ]; then
        sudo -A mount "$chosen" 2>/dev/null && mnotify-send "💻 USB mounting" "$chosen mounted" && exit 0
    fi

	# mount other
	mp="/mnt/$(echo "$chosen" | sed 's/.*\///')"
	partitiontype="$(lsblk -no "fstype" "$chosen")"
    case "$partitiontype" in
        "vfat") sudo -A mkdir -p $mp && sudo -A mount -t vfat "$chosen" "$mp" -o rw,umask=0000 || mountFailed "$chosen" ;;
        *)      sudo -A mkdir -p $mp && sudo -A mount "$chosen" "$mp" || mountFailed "$chosen"
                user="$(whoami)"
                ug="$(groups | awk '{print $1}')"
                sudo -A chown "$user":"$ug" "$mp" ;;
    esac
    notify-send "💻 USB mounting" "$chosen mounted to $mp"
}


# MAIN
usbDrives=$(lsblk -P -p -o "NAME,FSTYPE,SIZE,TYPE,MOUNTPOINT,RM" | grep 'RM="1"' | grep 'TYPE="part"' | grep 'MOUNTPOINT=""' | awk '{printf "%s %s\n",$1,$3}' | sed -e 's/NAME="//g' | sed -e 's/SIZE="//g' | sed -e 's/"//g' | awk '{printf "%s (%s)\n",$1,$2}')
[ -z "$usbDrives" ] && notify-send "💻 USB mounting" "No USB drive detected" && exit
mountUsb "$usbDrives"


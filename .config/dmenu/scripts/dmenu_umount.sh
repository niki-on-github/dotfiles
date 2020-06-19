#!/usr/bin/env sh
# Description: A dmenu prompt to unmount drives.

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""
[ -z "$SUDO_ASKPASS" ] && notify-send "Warning" "Environment variable \$SUDO_ASKPASS is not set!"

unmountUsbFailed() {
    notify-send "💻 USB unmounting failed" "$1 could not be unmount" && exit 1
}

unmountUsb() {
	[ -z "$drives" ] && exit
	chosen=$(echo "$drives" | eval "dmenu -i -p \"Unmount drive >\" $DMENU_STYLE" | awk '{print $1}') || exit
    sudo -A umount "$chosen" || unmountUsbFailed "$chosen"
    sudo -A rm -f -d $chosen # delete folder if empty
    notify-send "💻 USB unmounting" "$chosen unmounted"
}

unmountAndroidFailed() {
    notify-send "💻 Android unmounting failed" "$1 could not be unmount" && exit 1
}

unmountAndroid() {
    chosen=$(awk '/mtpfs/ {print $2}' /etc/mtab | eval "dmenu -i -p \"Unmount device >\" $DMENU_STYLE") || exit
    sudo -A umount -l "$chosen" || unmountAndroidFailed "$chosen"
    sudo -A rm -f -d $chosen # delete folder if empty
    notify-send "💻 Android unmounting" "$chosen unmounted"
}

asktype() {
    chosen="$(printf "USB\\nAndroid" | eval "dmenu -i -p \"Unmount >\" $DMENU_STYLE")" || exit
	case $chosen in
		USB)        unmountUsb ;;
		Android)    unmountAndroid ;;
	esac
}


# MAIN
drives=$(lsblk -nrpo "name,type,size,mountpoint" | awk '$2=="part"&&$4!~/\/boot|\/home$|SWAP/&&length($4)>1{printf "%s (%s)\n",$4,$3}')
if ! grep mtpfs /etc/mtab; then
	[ -z "$drives" ] && notify-send "💻 USB unmounting" "No drives to unmount" && exit
	unmountUsb
else
	if [ -z "$drives" ]; then
        unmountAndroid
	else
        asktype
	fi
fi

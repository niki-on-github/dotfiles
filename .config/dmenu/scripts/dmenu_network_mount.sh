#!/usr/bin/env sh
# Description: Use dmenu to mount network drives
# Dependencies: xdialog, sshfs, cifs-utils

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""
[ -z "$SUDO_ASKPASS" ] && notify-send "Warning" "Environment variable \$SUDO_ASKPASS is not set!"

MOUNT_PATH="/home/$USER/MOUNT/"


smb_mount() {
    SMB_PATH=$(Xdialog --stdout --wmclass "pop-up" --title "samba path" --left --inputbox "samba path: (syntax: //<ip>/<path>)" 14 60)
    [ -z "$SMB_PATH" ] && exit

    MOUNT_POSTFIX=${SMB_PATH//\/\//}
    MOUNT_POSTFIX=${MOUNT_POSTFIX//\//_}
    MOUNT_POSTFIX=${MOUNT_POSTFIX//./-}
    MOUNT_PATH="${MOUNT_PATH}smb_${MOUNT_POSTFIX}"

    if [ -d "$MOUNT_PATH" ]; then
        [ ! -z "$(ls -A $MOUNT_PATH)" ] && notify-send "Network Mount" "Mount directory alredy in use" && exit
    else
        mkdir -p $MOUNT_PATH
    fi

    result=$(Xdialog --stdout --wmclass "pop-up" --title "Samba Credential" --left --password=2 --2inputsbox "Samba Credential" 20 50 "Samba User" "admin" "Samba Password" "" )
    [ -z "$result" ] && exit

    SMB_USR=$(echo $result | cut -d "/" -f1)
    SMB_PW=$(echo $result | cut -d "/" -f2)

    if ! sudo -A mount -t cifs -o uid=$(id -u),gid=$(id -g),user=$SMB_USR,password=$SMB_PW "$SMB_PATH" "$MOUNT_PATH" ; then
        notify-send "Network Mount" "Samba mount FAILED"
    else
        notify-send "Network Mount" "Samba mount to $MOUNT_PATH"
    fi

    clear && exit
}


smb_umount() {
    umountSmbDirectory=$(ls -A $MOUNT_PATH | grep "^smb_" | eval "dmenu -i -p \"SMB umount >\" $DMENU_STYLE") || exit

    MOUNT_PATH="${MOUNT_PATH}${umountSmbDirectory}"
    if sudo -A umount ${MOUNT_PATH} ; then
        # del folder if empty
        sudo -A rm -f -d ${MOUNT_PATH}
        notify-send "Network Mount" "Samba drive umounted"
    else
        notify-send "Network Mount" "Samba umount FAILED"
    fi

    clear && exit
}


sftp_mount() {
    notify-send "Warning" "SFTP Fuse Mount is unstable"
    SFTP_PATH=$(Xdialog --stdout --wmclass "pop-up" --title "sftp path" --left --inputbox "sftp path: (syntax: <ip>:<path>)" 14 60)
    [ -z "$SFTP_PATH" ] && exit

    MOUNT_POSTFIX=${SFTP_PATH//\/\//}
    MOUNT_POSTFIX=${MOUNT_POSTFIX//\//_}
    MOUNT_POSTFIX=${MOUNT_POSTFIX//./-}
    MOUNT_PATH="${MOUNT_PATH}sftp_${MOUNT_POSTFIX}"

    if [ -d "$MOUNT_PATH" ]; then
        [ ! -z "$(ls -A $MOUNT_PATH)" ]; notify-send "Network Mount" "Mount directory alredy in use" && exit
    else
        mkdir -p $MOUNT_PATH
    fi

    result=$(Xdialog --stdout --wmclass "pop-up" --title "SSH Credential" --left --password=2 --2inputsbox "SSH Credential" 20 50 "SSH User" "root" "SSH Password" "" )
    [ -z "$result" ] && exit

    SFTP_USR=$(echo $result | cut -d "/" -f1)
    SFTP_PW=$(echo $result | cut -d "/" -f2)

    if ! echo "$SFTP_PW" | sshfs -o password_stdin ${SFTP_USR}@${SFTP_PATH} $MOUNT_PATH ; then
        notify-send "Network Mount" "SFTP Mount Failed"
    else
        notify-send "Network Mount" "SFTP mount to $MOUNT_PATH"
    fi

    clear && exit
}


sftp_umount() {
    umountSftpDirectory=$(ls -A $MOUNT_PATH | grep "^sftp_" | eval "dmenu -i -p \"SFTP umount >\" $DMENU_STYLE") || exit

    MOUNT_PATH="${MOUNT_PATH}${umountSftpDirectory} "
    if umount ${MOUNT_PATH} ; then
        # del folder if empty
        rm -f -d ${MOUNT_PATH}
    else
        notify-send "Network Mount" "SFTP umount failed"
    fi

    clear && exit
}


# MAIN

[ ! -d "$MOUNT_PATH" ] && mkdir -p $MOUNT_PATH


menu=()
menu+=("smb mount")
menu+=("sftp mount")

if [ "$(ls -A $MOUNT_PATH | grep '^smb_')" ]; then
    menu+=("smb unmount")
fi

if [ "$(ls -A $MOUNT_PATH | grep '^sftp_')" ]; then
    menu+=("sftp unmount")
fi

# make sure it ends with /
if ! echo "$MOUNT_PATH" | grep "/$" >/dev/null ; then
    MOUNT_PATH="$MOUNT_PATH/"
fi

choice=$(printf '%s\n' "${menu[@]}" | eval "dmenu -i -p \"mount >\" $DMENU_STYLE") || exit
case $choice in
    "smb mount")    smb_mount ;;
    "smb unmount")  smb_umount ;;
    "sftp mount")   sftp_mount ;;
    "sftp unmount") sftp_umount ;;
esac


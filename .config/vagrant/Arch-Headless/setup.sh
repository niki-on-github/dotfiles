#!/bin/sh

LOCALE="de_DE.UTF-8"
MIRROR="Germany"
LANG="de_DE.UTF-8"
KEYMAP="de-latin1"
X11_KEYMAP="de pc105"
LOCALTIME="/usr/share/zoneinfo/Europe/Berlin"
HOSTNAME="vagrant-arch"

setup_timezone() {
    rm -f /etc/localtime
    ln -sf $LOCALTIME /etc/localtime
    timedatectl set-ntp true
}

setup_locale() {
    sed -i 's/#'"$LOCALE"' UTF-8/'"$LOCALE"' UTF-8/g' /etc/locale.gen
    locale-gen
    echo "LANG=$LANG" > /etc/locale.conf
    export LANG=$LANG
    echo "KEYMAP=$KEYMAP" > /etc/vconsole.conf
}

setup_pacman() {
    sed -i "s/^#Color/Color/" /etc/pacman.conf
    sed -i "s/^#ParallelDownloads.*$/ParallelDownloads = 4/g" /etc/pacman.conf
    pacman-key --populate archlinux
    pacman -Syy --noconfirm archlinux-keyring
    pacman -S --noconfirm --needed reflector
    # reflector --country "$MIRROR" --age 10 --protocol https --sort rate --save /etc/pacman.d/mirrorlist
    pacman --noconfirm -Syyu
}

resize_disk() {
    pacman -S --noconfirm --needed parted expect
    root_disk=$(cat /proc/mounts | grep " / " | cut -d ' ' -f 1 | head -n 1)
    if echo "$root_disk" | grep -v -q "/dev/vda" ; then
        expect -c "spawn parted $root_disk; send \"resizepart 2 -1\rYes\r-1\rq\r\"; expect eof"
        resize2fs $root_disk
    elif [ -e "/dev/vda2" ]; then
        expect -c "spawn fdisk /dev/vda; send \"d\r2\rn\r2\r\r\rN\rw\r\"; expect eof"
        partprobe /dev/vda
        if cat /proc/mounts | grep " / " | grep -q btrfs; then
            btrfs filesystem resize max /
        else
            resize2fs /dev/vda2
        fi
    fi
}

setup_linux() {
    pacman --noconfirm --needed -S linux linux-headers sudo haveged
    systemctl enable haveged
}

network_settings() {
   echo "$HOSTNAME" > /etc/hostname
   echo "127.0.0.1 localhost" >> /etc/hosts
   echo "::1 localhost" >> /etc/hosts
   echo "127.0.0.1 $HOSTNAME.local $HOSTNAME" >> /etc/hosts
   pacman --noconfirm --needed -S networkmanager net-tools
   systemctl enable NetworkManager.service
}

virtmanager_guest_utils() {
    pacman -S --noconfirm --needed spice-vdagent qemu-guest-agent
    systemctl enable qemu-guest-agent.service
}


# MAIN
setup_timezone
setup_locale
setup_pacman
resize_disk
setup_linux
network_settings
virtmanager_guest_utils
exit 0

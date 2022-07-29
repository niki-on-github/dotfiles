#!/bin/sh

LOCALE="de_DE.UTF-8"
MIRROR="Germany"
LANG="de_DE.UTF-8"
KEYMAP="de-latin1"
X11_KEYMAP="de pc105"
LOCALTIME="/usr/share/zoneinfo/Europe/Berlin"
KEYSERVER="pool.sks-keyservers.net"
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
    pacman-key --populate archlinux
    pacman -Syy --noconfirm archlinux-keyring

    if grep "^keyserver " /etc/pacman.d/gnupg/gpg.conf >/dev/null 2>&1 ; then
        sed -i 's/^keyserver .*$/keyserver '$(echo "$KEYSERVER" | sed 's/\//\\\//g')'/g' /etc/pacman.d/gnupg/gpg.conf
    else
        mkdir -p /etc/pacman.d/gnupg
        echo "keyserver $KEYSERVER" >> /etc/pacman.d/gnupg/gpg.conf
    fi

    pacman-key --refresh-keys
    pacman -S --noconfirm --needed reflector
    reflector --country "$MIRROR" -l 30 --sort rate --save /etc/pacman.d/mirrorlist
    pacman --noconfirm -Syyu
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

setup_xfce() {
    pacman -S --noconfirm --needed xfce4 xfce4-clipman-plugin thunar-archive-plugin xfce4-pulseaudio-plugin xfce4-notifyd ristretto gvfs gvfs-smb sshfs pulseaudio pavucontrol xorg-server network-manager-applet networkmanager-openvpn xorg-xrandr engrampa cronie gnome-keyring
    systemctl enable cronie.service
    pacman -S --noconfirm --needed lightdm lightdm-gtk-greeter
    systemctl enable -f lightdm
    localectl set-keymap $KEYMAP
    localectl set-x11-keymap $X11_KEYMAP
    localectl set-locale LANG=$LANG
}

virtualbox_guest_utils() {
    pacman -R --noconfirm virtualbox-guest-utils-nox # remove VirtualBox Guest utilities without X support
    pacman -S --noconfirm virtualbox-guest-utils virtualbox-guest-dkms # install VirtualBox Guest utilities with X support
    groupadd vboxusers
    groupadd vboxsf
    gpasswd -a vagrant vboxusers
    gpasswd -a vagrant vboxsf
    systemctl enable vboxservice.service
}

resize_disk_virtualbox() {
    pacman -S --noconfirm --needed parted expect
    expect -c "spawn parted /dev/sda; send \"resizepart 2 -1\rYes\r-1\rq\r\"; expect eof"
    resize2fs /dev/sda2
}

# MAIN
setup_timezone
setup_locale
setup_pacman
setup_linux
network_settings
setup_xfce
virtualbox_guest_utils
resize_disk_virtualbox

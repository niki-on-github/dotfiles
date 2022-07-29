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
    # reflector --country "$MIRROR" -l 30 --sort rate --save /etc/pacman.d/mirrorlist
    pacman --noconfirm -Syyu
}

resize_disk() {
    pacman -S --noconfirm --needed parted expect
    root_disk=$(cat /proc/mounts| grep " / " | cut -d ' ' -f 1 | head -n 1)
    if echo "$root_disk" | grep -v -q "/dev/vda" ; then
        expect -c "spawn parted $root_disk; send \"resizepart 2 -1\rYes\r-1\rq\r\"; expect eof"
        resize2fs $root_disk
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

add_black_arch_repo() {
    pushd /tmp
    pacman --noconfirm --needed -S curl
    curl -O https://blackarch.org/strap.sh
    chmod +x strap.sh
    ./strap.sh
    popd
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
add_black_arch_repo
setup_xfce
virtualbox_guest_utils
virtmanager_guest_utils
exit 0

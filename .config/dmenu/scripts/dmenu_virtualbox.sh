#!/bin/bash
# Description: Start VirtualBox VM from dmenu
# NOTE: In wayland you have to call this script with QT_QPA_PLATFORM=xcb Environment Variable
# Expample: `bindsym $mod+Shift+v exec QT_QPA_PLATFORM=xcb  ~/.config/dmenu/scripts/dmenu_virtualbox.sh`

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""
[ "$XDG_SESSION_TYPE" = "wayland" ] && [ "$QT_QPA_PLATFORM" != "xcb" ] && notify-send "VirtualBox" "Environment Variable QT_QPA_PLATFORM is missing" && exit 1
[ -n "$(pgrep qemu-system)" ] && notify-send "ERROR" "KVM and VirtualBoxVM cannot run at the same time" && exit 1

vm=$(vboxmanage list vms | grep -v "inaccessible" | cut -d '"' -f 2 | sort | eval "dmenu -i -p \"VirtualBox Start VM >\" $DMENU_STYLE") || exit

vmMemorySizeMB=$(vboxmanage showvminfo "$vm" | awk '/^Memory size/{print $3}')
vmMemorySizeMB="${vmMemorySizeMB//MB/}" #remove MB
vmMemorySizeMB=$(expr $vmMemorySizeMB + 256) #additional safety measure

#NOTE: 'free' use system language -> replace "Speicher" to the output of your language
freeMemorySizeMB=$(free -m | awk '/^Speicher:/{print $7}') #get free RAM in MB,

if [ "$freeMemorySizeMB" -gt "$vmMemorySizeMB" ]; then
    vboxmanage startvm "$vm" 2>&1 | grep "already locked by a session" && notify-send "VirtualBox" "VM \"$vm\" already running"
else
    notify-send "VirtualBox" "Not enought memmory available"
fi

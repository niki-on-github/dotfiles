#!/bin/bash
# Description: Start KVM with Looking Glass from dmenu

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""
[ -z "$XDG_RUNTIME_DIR" ] && notify-send "ERROR" "Please run script with sudo -E" && exit 1
[ -n "$(pgrep -x VirtualBoxVM)" ] && notify-send "ERROR" "KVM and VirtualBoxVM cannot run at the same time" && exit 1

lookingGlassVMs=()
while read -r line ; do
    if virsh --connect qemu:///system dumpxml --domain "$line" | grep "looking-glass" >/dev/null ; then
        lookingGlassVMs+=("$line")
    fi
done <<<$(virsh --connect qemu:///system list --all --name | grep -v "^$" )
[ "${#lookingGlassVMs[@]}" -le "0" ] && exit

vm=$(printf '%s\n' "${lookingGlassVMs[@]}"| sort | eval "dmenu -i -p \"Start KVM >\" $DMENU_STYLE") || exit
[ -z "$vm" ] && exit

# NOTE: uncomment to autoshutdown at exit
# close() { virsh --connect qemu:///system shutdown $vm >/dev/null 2>&1; }
# trap close SIGHUP SIGINT SIGTERM EXIT

if ! virsh --connect qemu:///system list --state-running --name | grep "^${vm}$" >/dev/null ; then
    touch /dev/shm/looking-glass
    chown $USER:kvm /dev/shm/looking-glass
    chmod 660 /dev/shm/looking-glass

    msg=$(virsh --connect qemu:///system start $vm 2>&1)
    retCode=$?
    notify-send "KVM" "$(grep -v \"^$\" <<< $msg)"
    [ "$retCode" = "0" ] || exit 1
    sleep 28
fi

notify-send "KVM" "Host Key set to: R-CTRL"
sleep 2

spicePort=$(virsh --connect qemu:///system domdisplay $vm | cut -d ':' -f3)

# NOTE: press Host Key to enable mouse capture (in xmonad this only work without Fullscreen option -F!)
looking-glass-client -S -m 228 -p $spicePort

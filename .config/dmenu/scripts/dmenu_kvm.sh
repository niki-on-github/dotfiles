#!/bin/bash
# Description: Start KVM from dmenu


VIEWER="virt-manager" # default viewer
START_NETWORK=1

CACHE_FILE="$HOME/.cache/dmenu_kvm"

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""
[ -n "$(pgrep -x VirtualBoxVM)" ] && notify-send "ERROR" "KVM and VirtualBoxVM cannot run at the same time" && exit 1

if [ ! -f $CACHE_FILE ]; then
    virsh --connect qemu:///system list --all --name | grep -v "^$" | sort > $CACHE_FILE
fi

VM=$(cat $CACHE_FILE | eval "dmenu -i -p \"Start KVM >\" $DMENU_STYLE") || exit

# update cache file
virsh --connect qemu:///system list --all --name | grep -v "^$" | sort > $CACHE_FILE &

[ -z "$VM" ] && exit

xmldump=$(virsh --connect qemu:///system dumpxml --domain "$VM")

if echo "$xmldump" | grep -q "looking-glass" ; then
    VIEWER="looking-glass"
fi

STARTUP=0
if ! virsh --connect qemu:///system list --state-running --name | grep -q "^${VM}$" ; then
    STARTUP=1

    if [ "$VIEWER" = "looking-glass" ]; then
        touch /dev/shm/looking-glass
        chown $USER:kvm /dev/shm/looking-glass
        chmod 660 /dev/shm/looking-glass
    fi

    if [ $START_NETWORK -eq 1 ]; then
        if echo "$xmldump" | grep -q "<interface type='network'>" ; then
            interface=$(echo "$xmldump" | grep -Eo "<source network=['\"][^'\"]*'")
            interface=${interface:17:-1}
            virsh --connect qemu:///system net-start $interface >/dev/null 2>&1
        fi
    fi

    msg=$(virsh --connect qemu:///system start $VM 2>&1)
    retCode=$?
    notify-send "KVM" "$(grep -v \"^$\" <<< $msg)"
    [ "$retCode" = "0" ] || exit 1
fi


if ! echo "$xmldump" | grep -q "<channel type='spicevmc'>" ; then
    VIEWER="headless"
fi

if [ "$VIEWER" = "headless" ]; then
    if [ $STARTUP -eq 1 ]; then
        echo "wait for network dhcp ip lease..."
        sleep 9
    fi
    netInfo=""
    while [ -z "$netInfo" ]; do
        sleep 1
        netInfo=$(virsh --connect qemu:///system domifaddr $VM | tail -n +3)
        if [ "$?" != "0" ]; then
            notify-send "KVM" "$(virsh --connect qemu:///system domifaddr $VM 2>&1)"
            exit 1
        fi
    done

    notify-send "$VM" "IPv4: $(echo $netInfo | cut -d ' ' -f 4)"
    ipv4=$(echo $netInfo | cut -d ' ' -f 4 | cut -d '/' -f 1)

    if grep -q "$ipv4" $HOME/.ssh/config ; then
        # open terminal with ssh session if user is set in ~/.ssh/config
        exec eval "$TERMINAL -t \"ssh $ipv4\" -e ssh $ipv4"
    else
        # else copy ipv4 to clipboard
        echo "$ipv4" | tr -d '\n' | xclip -selection clipboard
        echo "$ipv4" | tr -d '\n' | xclip
    fi

elif [ "$VIEWER" = "looking-glass" ] ; then
    if [ $STARTUP -eq 1 ]; then
        echo "wait for guest to boot up..."
        sleep 28
        notify-send "KVM" "Host Key set to: R-CTRL"
        sleep 2
    fi
    spicePort=$(virsh --connect qemu:///system domdisplay $VM | cut -d ':' -f3)
    echo "spice port: $spicePort"
    looking-glass-client -S -m 228 -p $spicePort & disown

elif [ "$VIEWER" = "remote-viewer" ]; then
    display=$(virsh --connect qemu:///system domdisplay $VM)
    notify-send "KVM" "Open Display: $display"
    remote-viewer $display & disown

elif [ "$VIEWER" = "virt-manager" ]; then
    virt-manager --connect qemu:///system --show-domain-console $VM & disown

else
    notify-send "ERROR" "Viewer $VIEWER not implemented"
fi

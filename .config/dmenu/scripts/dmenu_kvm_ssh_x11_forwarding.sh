#!/bin/bash
# Description: Start KVM with SSH X11 forwarding and Xephyr from dmenu

DEFAULT_SSH_USER="vagrant"
# DEFAULT_SSH_USER="manjaro"

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""
[ -n "$(pgrep -x VirtualBoxVM)" ] && notify-send "ERROR" "KVM and VirtualBoxVM cannot run at the same time" && exit 1

VM=$(virsh --connect qemu:///system list --all --name | grep -v "^$" | sort | eval "dmenu -i -p \"Start KVM >\" $DMENU_STYLE") || exit
[ -z "$VM" ] && exit

# if kvm is not running
if ! virsh --connect qemu:///system list --state-running --name | grep "^${VM}$" >/dev/null ; then
    msg=$(virsh --connect qemu:///system start $VM 2>&1)
    retCode=$?
    # example code to start network: virsh --connect qemu:///system net-start vagrant-libvirt
    notify-send "KVM" "$(grep -v \"^$\" <<< $msg)"
    [ "$retCode" = "0" ] || exit 1
    sleep 6
fi

netInfo=$(virsh --connect qemu:///system domifaddr $VM | tail -n +3)
if [ "$?" != "0" ]; then
    notify-send "KVM" "$(virsh --connect qemu:///system domifaddr $VM 2>&1)"
    exit 1
fi

for ((num=1 ; num <= 100 ; num++)) ; do
    if [ ! -e /tmp/.X11-unix/X$num ] && [ ":$num" != "$DISPLAY" ] ; then
        break
    fi
done
DISPNUM=":$num"

DISPRES=$(xdpyinfo  | grep 'dimensions:' | head -n1 | awk '{print $2}')
DISPWIDTH=$(echo "$DISPRES" | cut -d 'x' -f 1)
DISPHEIGHT=$(echo "$DISPRES" | cut -d 'x' -f 2)

# virtual display resolution, NOTE: resolution determines font scaling
VDISPWIDTH=`expr $DISPWIDTH - $DISPWIDTH / 9`
VDISPHEIGHT=`expr $DISPHEIGHT - $DISPHEIGHT / 9`

closeHook() {
    pkill Xephyr
}
trap closeHook SIGHUP SIGINT SIGTERM EXIT

echo "$netInfo" | while read line; do
    [ -z "$(echo $line | sed 's/ *//g')" ] && continue
    if [ "$(echo "$line" | awk '{print $1}')" != "-" ]; then

        VM_IP=$(echo $line | awk '{print $4}' | cut -d '/' -f 1)

        echo "wait for a ping success on $VM_IP"
        while ! ping -c 1 -n -w 1 $VM_IP &> /dev/null ; do
            sleep 1
        done
                echo "Create Display $DISPNUM"
        Xephyr $DISPNUM -br +extension randr -resizeable -screen ${VDISPWIDTH}x${VDISPHEIGHT} &
        if grep -q "$VM_IP" $HOME/.ssh/config ; then
            # maybe we use a difrent ssh port so we wait a predefined time to make sure the server is up
            sleep 6
            DISPLAY="$DISPNUM" ssh -o ForwardX11Timeout=2147423s -X $VM_IP 'startxfce4 || startplasma-x11'
        else
            while ! nmap $VM_IP -p 22 | grep -q "22/tcp open" ; do
                sleep 1
            done

            DISPLAY="$DISPNUM" ssh -o ForwardX11Timeout=2147423s -X $DEFAULT_SSH_USER@$VM_IP 'startxfce4 || startplasma-x11'
        fi
        break
    fi
done

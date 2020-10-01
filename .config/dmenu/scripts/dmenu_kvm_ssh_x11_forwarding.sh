#!/bin/bash
# Description: Start KVM with SSH X11 forwarding and Xephyer from dmenu

SSH_DESKTOP="startxfce4"
SSH_USER="vagrant"

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
    sleep 9
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

echo "$netInfo" | while read line; do
    [ -z "$(echo $line | sed 's/ *//g')" ] && continue
    if [ "$(echo "$line" | awk '{print $1}')" != "-" ]; then
        VM_IP=$(echo $line | awk '{print $4}' | cut -d '/' -f 1)

        echo "Create Display $DISPNUM"
        Xephyr $DISPNUM -br +extension randr -resizeable -screen ${VDISPWIDTH}x${VDISPHEIGHT} &
        #DISPLAY="$DISPNUM" ssh -Y $SSH_USER@$VM_IP $SSH_DESKTOP
        DISPLAY="$DISPNUM" ssh -o ForwardX11Timeout=2147423s -X $SSH_USER@$VM_IP $SSH_DESKTOP

        break
    fi
done


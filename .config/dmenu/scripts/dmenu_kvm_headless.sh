#!/bin/bash
# Description: Start KVM with SSH from dmenu

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
    sleep 9 # wait for network ip
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

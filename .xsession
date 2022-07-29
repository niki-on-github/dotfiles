#!/bin/sh

if grep -q "^flags.* hypervisor" /proc/cpuinfo ; then
    $HOME/.local/bin/x11-vm-autoresize &
endif

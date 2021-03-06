#!/bin/bash
# Description: Use dmenu to select a process and a signal to send to it

# colors
IFS=':' read nb nf sb sf <<< "dark red:white:red:white"

# get process tree and list of signals
tree=$(ps --forest -u "$USER" -o "pid args" | sed '1d')
list=$(kill -l | tr '\t' '\n')

# prompt for process
pid=$(dmenu -i -l 30 -nb "$nb" -nf "$nf" -sb "$sb" -sf "$sf" <<< "$tree" | awk '{print $1}')
if [ -z "$pid" ]; then
    exit 1;
fi

# prompt for signal
sig=$(dmenu -i -nb "$nb" -nf "$nf" -sb "$sb" -sf "$sf" <<< "$list" | awk '{print $2}')
if [ -z "$sig" ]; then
    exit 1;
fi

# lookup name
name=$(ps -p "$pid" -o comm --no-headers)

# send signal to process
kill "-$sig" "$pid"
if [ $? ]; then
    printf "%s'd %s (%s)" "$sig" "$name" "$pid"
else
    printf "Failed to kill %s ( %s )" "$name" "$pid"
    exit  1
fi

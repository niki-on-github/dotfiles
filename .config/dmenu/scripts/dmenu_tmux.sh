#!/bin/bash
# Description: dmenu selection of tmux sessions

if tmux list-sessions >/dev/null 2>&1; then
    # select a session
    list=$(tmux list-sessions | dmenu -l 16) || exit
    [ -z "$list" ] && exit
    sess=$(awk -F: '{print $1}' <<< "$list")

    # launch an xterm loading the tmux session
    $TERMINAL -e tmux attach-session -t "$sess"
else
    notify-send "tmux" "no server running"
fi

#!/bin/sh
# Description: .xinitrc runs wehen you type in startx

# There are some environment variables which are set by a login manager. We have to do this manually.
export XDG_SESSION_TYPE="x11"
export DESKTOP_SESSION="dwm"

# There are some important commands that need to be run when we start the graphical environment. 
# I keep those commands in ~/.xprofile because that file is run automatically from a display manager. 
# To prevent doubling up commands, I source them here.
[ -f $HOME/.xprofile ] && . $HOME/.xprofile

# start the graphical environment
exec dwm

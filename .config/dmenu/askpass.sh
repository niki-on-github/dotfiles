#!/usr/bin/env sh

# Set this script in the SUDO_ASKPASS environment variable to redirect sudo passwords form 'sudo -A' to this dmenu password promt.
# NOTE: This script required dmenu with password promt patch (-P)

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""

# NOTE: We override the sb color from DMENU_STYLE
pw=$(eval "dmenu -p \"$1\" -P $DMENU_STYLE -sb \"#cc0000\"")

if [ -z "$pw" ]; then
    echo "no password was entered" >&2
    exit 1
else
    echo "$pw"
    echo "" >&2
fi

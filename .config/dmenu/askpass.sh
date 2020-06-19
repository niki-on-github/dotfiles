#!/usr/bin/env sh

# Set this script in the SUDO_ASKPASS environment variable to redirect sudo passwords form 'sudo -A' to this dmenu password promt.
# NOTE: This script required dmenu with password promt patch (-P)

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""

# NOTE: We override the sb color from DMENU_STYLE
eval "dmenu -p \"$1\" -P $DMENU_STYLE -sb \"#cc0000\""

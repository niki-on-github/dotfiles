#!/bin/sh
# Detects the width of running systray window and creates an XPM icon
# of that width, 1px height, and transparent.
# Outputs an <icon>-tag for use in xmobar to display the generated
# XPM icon.
#
# Run script from xmobar:
# `Run Com "/bin/sh" ["-c", "$HOME/.config/xmobar/systray-padding.sh"] "systray" 10`
# and use `%systray%` in your template.

TRAY_NAME="panel"  # trayer
# if pgrep stalonetray >/dev/null; then
#     TRAY_NAME="stalonetray"
# else
#     TRAY_NAME="panel"  # trayer
# fi

# Function to create a transparent Wx1 px XPM icon
create_xpm_icon () {
    timestamp=$(date)
    pixels=$(for i in `seq $1`; do echo -n "."; done)

    cat << EOF > "$2"
/* XPM *
static char * trayer_pad_xpm[] = {
/* This XPM icon is used for padding in xmobar to */
/* leave room for trayer-srg. It is dynamically   */
/* updated by by trayer-pad-icon.sh which is run  */
/* by xmobar.                                     */
/* Created: ${timestamp} */
/* <w/cols>  <h/rows>  <colors>  <chars per pixel> */
"$1 1 1 1",
/* Colors (none: transparent) */
". c none",
/* Pixels */
"$pixels"
};
EOF
}

# Width of the systray window
width=$(xprop -name "$TRAY_NAME" | grep 'program specified minimum size' | cut -d ' ' -f 5)
[ -z "$width" ] && width=0

# Icon file name
iconfile="/tmp/xmobar-systray-padding-${width}px.xpm"

# If the desired icon does not exist create it
if [ ! -f $iconfile ]; then
    create_xpm_icon $width $iconfile
fi

# Output the icon tag for xmobar
echo "<icon=${iconfile}/>"

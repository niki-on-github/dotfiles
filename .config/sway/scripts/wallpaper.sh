#!/bin/sh
# Description: Helper Script to set the Wallpaper

[ -z "$WALLPAPER_PATH" ] && WALLPAPER_PATH="$HOME/.local/share/wallpaper"
[ -d "$WALLPAPER_PATH" ] || WALLPAPER_PATH="$HOME/.local/share/wallpaper"


usage () {
    cat <<EOF
'`basename $0`' Helper Script to set the Wallpaper

Dependecies: - feh
             - sxiv

Usage: $0 choice|--choice|-c
       $0 random|--random|-r
       $0 help|--help|-h

The following specific options are supported:

  -c, --choice      Shows a selection menu with all background images from the $WALLPAPER_PATH folder
  -r, --random      Selects a random wallpaper from $WALLPAPER_PATH folder
  -h, --help        Display this help

EOF
    exit $1
}

random() {
    bg=$(find $WALLPAPER_PATH -type f 2>/dev/null | grep -v " " | shuf -n1)
    [ -n "$bg" ] && swaymsg "output * bg $bg fill"
}

choice() {
    bg=$(find $WALLPAPER_PATH -type f 2>/dev/null | grep -v " " | shuf | sxiv -aiobqtf 2>/dev/null | head -n1)

    if [ -z "$bg" ]; then
        notify-send "INFO" "No background image was selected (choose random)"
        random
    else
        swaymsg "output * bg $bg fill"
    fi
}

# MAIN
case "$1" in
    choice|--choice|-c)     choice ;;
    random|--random|-r)     random ;;
    help|--help|-h)         usage 0 ;;
    *)                      usage 1 ;;
esac

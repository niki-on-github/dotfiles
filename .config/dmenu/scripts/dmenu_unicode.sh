#!/usr/bin/env sh
# Description: Get a menu of emojis to clipboard
# Dependencies: noto-fonts-emoji

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""
command -v xclip >/dev/null || ( notify-send "ERROR" "xclip not available" && exit )
[ ! -f ~/.local/share/emoji ] && notify-send "ERROR" "FileNotFound: ~/.local/share/emoji" && exit

if command -v rofi >/dev/null ; then
    chosen=$(grep -v "#" ~/.local/share/emoji | eval "rofi -dmenu -columns 1 -p \"emoji2clip > \" -theme ~/.config/rofi/dmenu_list.rasi") || exit
else
    chosen=$(grep -v "#" ~/.local/share/emoji | eval "dmenu -i -l 20 -p \"emoji2clip > \" $DMENU_STYLE") || exit
fi

c=$(echo "$chosen" | sed "s/ .*//")
echo "$c" | tr -d '\n' | xclip -selection clipboard
s=$(echo "$chosen" | sed "s/.*; //" | awk '{print $1}')
echo "$s" | tr -d '\n' | xclip
notify-send "INFO" "'$c' copied to clipboard"

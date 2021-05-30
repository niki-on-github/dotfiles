#!/bin/bash
# Description: simple rbw dmenu wrapper (rbw is an unofficial bitwarden cli)

export PATH="$HOME/.cargo/bin/:$HOME/.local/bin/:$PATH"
[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""

if ! command -v rbw >/dev/null ; then
    notify-send "Bitwarden" "ERROR: rbw is not installed!"
    exit 1
fi

pw=$(rbw ls --fields folder,name,user \
    | sed 's/\t/\//g' \
    | sort \
    | eval "dmenu -i -l 15 -p \"Bitwarden >\" $DMENU_STYLE" \
    | sed 's/^[^\/]*\///' \
    | sed 's/\// /' \
    | xargs -r rbw get)

[ -z "$pw" ] && exit
echo "$pw" | tr -d '\n' | xclip -selection clipboard -l 1
notify-send "Bitwarden" "Password copied to clipboard"

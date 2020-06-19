#!/bin/env sh
# Description: Show notification on song change with album cover if exist.
# Dependencies: python-eyed3
# Setup: add `execute_on_song_change = /path/to/notify.sh` to your ncmpcpp config


_eyed3="/usr/bin/eyeD3"
[ -e $_eyed3 ] || notify-send "Warning" "Please install python-eyed3"


if [ -f ~/.config/mpd/mpd.conf ]; then
    MUSIC_DIR="$(grep "music_directory" ~/.config/mpd/mpd.conf | sed 's/^music_directory *"//g' | sed 's/" *//g')"
else
    MUSIC_DIR=$HOME/Musik/
fi

if ! echo "$MUSIC_DIR" | grep "/$" >/dev/null ; then
    MUSIC_DIR="$MUSIC_DIR/"
fi


# This function send notificaation without cover
fallback() {
    notify-send -i "$HOME/.config/ncmpcpp/music-icon.png" "Now Playing" "$(mpc current)" && exit
}

# This function try to extract cover from mp3 and send notification with cover
tryExtract() {
    [ ! -f "$1" ] && fallback
    [ ! -e $_eyed3 ] && fallback
    coverDir=$(mktemp -d)
    $_eyed3 --write-images="$coverDir" "$1" >/dev/null 2>&1 || fallback
    cover=$(ls -A $coverDir | head -n 1)
    [ -z "$cover" ] && rm -rf $coverDir && fallback
    notify-send -i "$coverDir/$cover" "Now Playing" "$(mpc current)"
    rm -rf $coverDir
    exit
}


# MAIN

# is song paused?
[ -z "$(mpc status | grep '\[paused\] ')" ] || exit

album="$(mpc --format %album% current)"
file="$(mpc --format %file% current)"
album_dir="${file%/*}"

# is player stoped?
[ -z "$album_dir" ] && exit

album_dir="${MUSIC_DIR}${album_dir}"
[ ! -d "$album_dir" ] && tryExtract "${MUSIC_DIR}${file}"

covers="$(find "$album_dir" -type d -exec find {} -maxdepth 1 -type f -iregex ".*/.*\(${album}\|cover\|folder\|artwork\|front\).*[.]\(jpe?g\|png\|gif\|bmp\)" \; )"
src="$(echo -n "$covers" | head -n1)"
[ -z "$src" ] && tryExtract "${MUSIC_DIR}${file}"

# If cover is a seperate image
notify-send -i "$src" "Now Playing" "$(mpc current)" && exit


#!/usr/bin/env sh

# WM Integation:
# - i3wm: add 'no_focus [instance="no-focus"]' to your i3 config
# - dwm: set noswallow=0 or nofocus=1 for class='no-focus'
# - xmonad: add 'className =? "no-focus" --> doF W.focusDown' to your windowRules in xmonad.hs config

# Link Handler Script FileType:
# - image) view in sxiv
# - video, music) view in mpv
# - otherwise) open link in browser


[ -z "$1" ] && { "$BROWSER" --class "no-focus"; exit; }

case "$1" in
	*youtube.com/watch*|*youtube.com/playlist*|*youtu.be*|*hooktube.com*|*bitchute.com*|*webm)
        # notify-send "Play Video" "$1"
		setsid mpv --x11-name="floating" --title="floating" --pause --profile=youtube --ytdl-format="bestvideo[height<=?480]+bestaudio/best" --speed=1.8 "$1" >/dev/null 2>&1 & ;;
    *mkv|*mp4)
        notify-send "Play Video" "$1"
        setsid mpv --x11-name="floating" --title="floating" --pause "$1" >/dev/null 2>&1 & ;;
	*png|*jpg|*jpeg|*gif)
        if grep -v "thumbs" <<< "$1" >/dev/null ; then
            notify-send "Preview Image" "$1"
            mkdir -p /tmp/newsboat/images
            fname="$(date +%+s).$(echo "$1" | sed "s/.*\.//")"
            pkill -f /tmp/newsboat/images  # kill old instance

            if grep "dwm$" <<< $DESKTOP_SESSION >/dev/null ; then
                # use dwm swallow to view image inplace
		        curl -sL "$1" > "/tmp/newsboat/images/$fname"
                # we store only images in this folder so we can use ls + grep to find the position of the image
                pos=$(ls "/tmp/newsboat/images" | sort | grep -Fn "$fname" | cut -d':' -f1 | head -n1) || pos=1
                setsid sxiv -N "no-focus" -n $pos -a "/tmp/newsboat/images"  >/dev/null 2>&1 &
            else
                setsid sh -c "curl -sL \"$1\" > /tmp/newsboat/images/$fname; sxiv -N no-focus -a /tmp/newsboat/images >/dev/null" &
            fi
        fi ;;
	*mp3|*flac|*opus|*mp3?source*)
        notify-send "Play Music" "$1"
		mpv "$1" ;;
	*)
		if [ -f "$1" ]; then
            "$TERMINAL" -e "$EDITOR $1"
		else
            setsid $BROWSER --class "no-focus" "$1" >/dev/null 2>&1 &
        fi ;;
esac

sleep 0.5
if [ "$(basename $DESKTOP_SESSION)" = "sway" ]; then
    swaymsg "[app_id=\"newsboat\"] focus"
else
    wmctrl -a newsboat
fi


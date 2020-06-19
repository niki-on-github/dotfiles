#!/usr/bin/env sh
# Description: Use dmenu with mpc to control mpd
# Dependencies: dmenu, mpc, (mpd)

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""


crossfade() {
    local crossfade=`echo -e "0\n3\n5" | eval "dmenu -i -p \"Crossfade >\" $DMENU_STYLE"`
    [ -z "$crossfade" ] || mpc crossfade $crossfade
}

consume() {
    local consume=`echo -e "on\noff" | eval "dmenu -i -p \"Consume >\" $DMENU_STYLE"`
    [ -z "$consume" ] || mpc consume $consume
}

random() {
    local random=`echo -e "on\noff" | eval "dmenu -i -p \"Random >\" $DMENU_STYLE"`
    [ -z "$random" ] || mpc random $random
}

repeat() {
    local repeat=`echo -e "on\noff" | eval "dmenu -i -p \"Repeat >\" $DMENU_STYLE"`
    [ -z "$repeat" ] || mpc repeat $repeat
}

single() {
    local single=`echo -e "on\noff" | eval "dmenu -i -p \"Single >\" $DMENU_STYLE"`
    [ -z "$single" ] || mpc single $single
}

#NOTE: Seeks by hour, minute or seconds, hours or minutes can be omitted. If seeking by percentage, seeks within the current song in the specified manner. If a "+" or "-" is used, the seek is done relative to the current song position. Absolute seeking by default.
seek() {
    local seek=`echo -e "0%\n+10%\n50%" | eval "dmenu -i -p \"Seek [+-][HH:MM:SS] or [+-][0-100]% >\" $DMENU_STYLE"`
    [ -z "$seek" ] || mpc seek "$seek"
}

search() {
    local artist=`mpc list albumartist | eval "dmenu -i -p \"Artist >\" $DMENU_STYLE"`
    [ -z "$artist" ] && exit
    local albumlist=`mpc list album artist "$artist"`
    local album=`echo -e "[ALL]\n$albumlist" | eval "dmenu -i -p \"Album >\" $DMENU_STYL"`
    [ -z "$album" ] && exit

    mpc clear
    if [[ $album == '[ALL]' ]]; then
        mpc find artist "$artist" | mpc add
    else
        mpc find artist "$artist" album "$album" | mpc add
    fi
    mpc play
}

playlist() {
    local track=`mpc playlist -f "%position% - %title% - %artist% - %album%" | eval "dmenu -i -l 5 -p \"Track >\" $DMENU_STYLE"`
    [ -z "$track" ] || mpc play "${track%% *}"
}

load() {
    local load=`mpc lsplaylists | eval "dmenu -i -l 5 -p \"Load >\" $DMENU_STYLE"`
    [ -z "$load" ] && exit
    mpc clear
    mpc load "$load" && mpc play
}


# MAIN
choice=`echo -e "Search\nPlaylist\nNext\nPause\nPlay\nPrev\nSeek\nRandom\nRepeat\nSingle\nConsume\nCrossfade\nLoad" | eval "dmenu -i -p \"Music Control >\" $DMENU_STYLE"`
if [ ! -z "$choice" ]; then
    case "$choice" in
        Search) search ;; # Open Search Menu
        Playlist) playlist ;; # Select a Song from current playlist

        Next) `mpc next` ;; # Starts playing next song on playlist
        Pause) `mpc pause` ;; # Pauses playing
        Play) `mpc play` ;; # Starts playing
        Prev) `mpc prev` ;; # Starts playing previous song
        Seek) seek ;; # Seeks the current song

        Random) random ;; # Set random mode
        Repeat) repeat ;; # Set repeat mode
        Single) single ;; # Set single mode
        Consume) consume ;; # Set consume mode
        Crossfade) crossfade ;; # Set the current amount of crossfading between songs

        Load) load ;; # Load a Playlist
    esac
fi

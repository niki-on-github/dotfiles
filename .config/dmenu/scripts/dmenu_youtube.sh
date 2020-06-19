#!/usr/bin/env sh
# NOTE: Add your favorite youtube channels to '~/.local/share/youtube'. Use one line per channel with the following structure: '<name>:www.youtube.com/channel/<channel>'
# NOTE: For newest video subscription use a RSS reader e.g. newsboat
# AUR Dependencies: pup-git


[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""

# Set your preferred video/audio format
YTDL_FORMAT="bestvideo[height<=?480]+bestaudio/best"

# HTML encoding on a string: 'encodedString=$(echo "StringToEncode" | sed $htmlEncoding)'
htmlEncoding='s/%/%25/g;s/\s/%20/g;s/!/%21/g;s/"/%22/g;s/#/%22/g;s/\$/%24/g;s/&/%26/g;s/\x27/%27/g;s/)/%29/g;s/(/%28/g;s/\*/%2A/g;s/\+/%2B/g;s/,/%2C/g;s/-/%2D/g;s/\./%2E/g;s/\//%2F/g;s/:/%3A/g;s/;/%3B/g;s/</%3C/g;s/=/%3D/g;s/>/%3E/g;s/\?/%3F/g;s/@/%40/g;s/]/%5D/g;s/\\/%5C/g;s/\[/%5B/g;s/\^/%5E/g;s/_/%5F/g;s/`/%60/g;s/}/%7D/g;s/|/%7C/g;s/{/%7B/g;s/~/%7E/g'


# MAIN
while true ; do
    # load channel list
    if [ ! -f ~/.local/share/youtube ]; then
        notify-send "INFO" "FileNotFound: ~/.local/share/youtube"
        channel="$(eval "dmenu -i -p \"youtube search >\" $DMENU_STYLE" < /dev/null)" || exit
    else
        channel="$(sort ~/.local/share/youtube | grep -v "^ *$" | sed -E '/^(#|$)/d' | sed 's/:.*//' | eval "dmenu -i -p \"youtube (channel|search) >\" $DMENU_STYLE")" || exit
    fi

    if  [ -f ~/.local/share/youtube ] && grep "^${channel}:" ~/.local/share/youtube >/dev/null 2>&1 ; then
        channel="$(grep "^${channel}:" ~/.local/share/youtube | head -1 | sed -e 's/https:\/\///g' | cut -d ':' -f 2)"
        if echo "$channel" | grep -v "/videos$" >/dev/null 2>&1 ; then
            if echo "$channel" | grep "/$" >/dev/null 2>&1 ; then
                channel="${channel}videos"
            else
                channel="${channel}/videos"
            fi
        fi
    else
        channel="www.youtube.com/results?search_query=$(echo "${channel}" | sed $htmlEncoding)"
    fi

    #NOTE: sometimes a download fails (therefore we try several times)
    requests=5
    while [ $requests -gt 0 ]; do
        requests=$(( $requests - 1 ))

        # get video list
        html="$(curl -s "https://$channel")"

        # search videos (title and url)
        title=$(echo "$html" | pup "h3.yt-lockup-title" | pup "a attr{title}")
        href=$(echo "$html" | pup "h3.yt-lockup-title" | pup "a attr{href}")

        # html download successful
        [ "$(echo "$title" | wc -l)" -gt "1" ] && break

        sleep 1
    done

    if [ "$(echo "$title" | wc -l)" -ne "$(echo "$href" | wc -l)" ] || [ "$(echo "$title" | wc -l)" -le "1" ] ; then
        notify-send "ERROR" "title.count() != url.count() or title.count() == 0 -> merge failed" && exit 1
    fi

    # select video to play
    selection="$(echo "$title" | grep -n "" | eval "dmenu -i -l 15 -p \"video >\" $DMENU_STYLE")" || continue
    [ -z "$(echo $selection | grep -E "^([0-9]+):")" ] && exit
    number="$(echo $selection | cut -d ':' -f 1)"

    # build video url and play
    videoUrl="https://www.youtube.com$(echo $href | cut -d ' ' -f ${number})"
    notify-send "Play Video" "$selection"
    exec mpv --ytdl-format="${YTDL_FORMAT}" "$videoUrl"
done


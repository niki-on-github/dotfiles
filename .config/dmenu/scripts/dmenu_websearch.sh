#!/usr/bin/env sh
# Description: A browser-independent address bar with bookmark support.
# Dependencies: dmenu, xdotool, hexdump, coreutils, wmctrl

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""


browser='firefox --new-window'
defaultEngine='https://duckduckgo.com/?q=%s'
bookmarks="$HOME/.local/share/bookmarks"
protocol='^(https?|ftps?|mailto|about|file):///?'

searchEngines=()
searchEngines+=( "arch-wiki:wiki.archlinux.org/index.php?title=Special%3ASearch&search=" )
searchEngines+=( "wikipedia:en.wikipedia.org/w/index.php?title=Special%3ASearch&search=" )
searchEngines+=( "duckDuckGo:duckduckgo.com/?t=hp&q=" )
searchEngines+=( "youtube:www.youtube.com/results?search_query=" )
searchEngines+=( "github:www.github.com/search?q=" )


# HTML encoding on a string: 'encodedString=$(echo "StringToEncode" | sed $htmlEncoding)'
htmlEncoding='s/%/%25/g;s/\s/%20/g;s/!/%21/g;s/"/%22/g;s/#/%22/g;s/\$/%24/g;s/&/%26/g;s/\x27/%27/g;s/)/%29/g;s/(/%28/g;s/\*/%2A/g;s/\+/%2B/g;s/,/%2C/g;s/-/%2D/g;s/\./%2E/g;s/\//%2F/g;s/:/%3A/g;s/;/%3B/g;s/</%3C/g;s/=/%3D/g;s/>/%3E/g;s/\?/%3F/g;s/@/%40/g;s/]/%5D/g;s/\\/%5C/g;s/\[/%5B/g;s/\^/%5E/g;s/_/%5F/g;s/`/%60/g;s/}/%7D/g;s/|/%7C/g;s/{/%7B/g;s/~/%7E/g'


gotourl() {
	if [ -n "$nbrowser" ]; then
        $nbrowser "$choice"
	else
        $browser "$choice"
	fi
}

searchweb() {
	#convert search query to percent encoding and insert it into url
	choice=$(echo "$choice" | hexdump -v -e '/1 " %02x"')
	choice=$(echo "$defaultEngine" | sed "s/%s/${choice% 0a}/;s/[[:space:]]/%/g")
	gotourl
}

checkurl() {
	grep -Fx "$choice" "$tmpfile" && choice=$(echo "$choice" | awk '{ print $1 }') && return 0
	[ ${#choice} -lt 4 ] && return 1
	echo "$choice" | grep -Z ' ' && return 1
	echo "$choice" | grep -EiZ "$protocol" && return 0
	echo "$choice" | grep -FZ '..' && return 1
	prepath=$(echo "$choice" | sed 's/(\/|#|\?).*//')
	echo "$prepath" |  grep -FvZ '.' && return 1
	echo "$prepath" |  grep -EZ '^([[:alnum:]~_:-]+\.?){1,3}' && return 0
}

# MAIN
# is firefox open on current desktop?
currentDesktop=$(xdotool get_desktop)
windowList=$(wmctrl -l -x | cut -c 13- | grep "^${currentDesktop}")
echo "$windowList" | awk '{print $2}' | grep "Navigator.firefox" && nbrowser='firefox'

tmpfile=$(mktemp /tmp/dmenu_websearch.XXXXXX)
trap 'rm "$tmpfile"' 0 1 15
printf '%s\n' "${searchEngines[@]}" | sort | sed 's/:.*//' > "$tmpfile"
[ -f "$bookmarks" ] && cat "$bookmarks" | sort >> "$tmpfile"
sed -i -E '/^(#|$)/d' "$tmpfile" #remove comments from bookmark file
choice=$(eval "dmenu -i -p \"GOTO WWW >\" $DMENU_STYLE" < "$tmpfile") || exit

# Use search engine
if printf '%s\n' "${searchEngines[@]}" | grep "^${choice}" ; then
	search=$(eval "dmenu -p \"Search >\" $DMENU_STYLE" < /dev/null) || exit
	searchEngineURL=$(printf '%s\n' "${searchEngines[@]}" | grep "^${choice}" | sed 's/.*://')
	choice="${searchEngineURL}$(echo $search | sed $htmlEncoding)"
	gotourl
	exit
fi

if checkurl; then
	echo "$choice" | grep -EivZ "$protocol" && choice="https://$choice"
	gotourl
else
    searchweb
fi


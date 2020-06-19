#!/bin/sh

# Description: A general handler for opening a file's intended output from vim, usually the pdf of a compiled document. 
# HowTo: Add 'map <leader>p :!/path/to/previewer.sh <c-r>%<CR><CR' to your vim config

basename="$(echo "$1" | sed 's/\.[^\/.]*$//')"

texPreview() {
    file=$(readlink -f "$1")
	dir=$(dirname "$file")
    [ "$dir" = "." ] && parentDir=".." || parentDir=$(dirname "$dir")
	base="${file%.*}"
    [ -f "${base}.pdf" ] && previewFile="${base}.pdf"

    if [ -f "${dir}/main.pdf" ]; then
        previewFile="${dir}/main.pdf"
    elif [ -f "${parentDir}/main.pdf" ]; then
        previewFile="${parentDir}/main.pdf"
    fi
    
    [ -z "$previewFile" ] && echo "Error: pdf not found" || setsid "$READER" "$previewFile" >/dev/null 2>&1 &
}

case "$1" in
    *tex) texPreview ;;
	*.md|*.[rR]md|*.ms|*.me|*.mom|*.MD) setsid "$READER" "$basename".pdf >/dev/null 2>&1 & ;;
	*.[0-9]) setsid "$READER" "$basename".pdf >/dev/null 2>&1 & ;;
	*.html) setsid $BROWSER "$basename".html >/dev/null 2>&1 & ;;
esac

#!/usr/bin/env bash
# Description: drop files and copy to current directory
# NOTE: lf has build in file drop support

command -v dragon-drag-and-drop >/dev/null ||( notify-send "Error" "Please install \"dragon-drag-and-drop\"" && exit 1 )


usage () {
    cat <<EOF
'`basename $0`' drop files and copy to current directory


Dependecies: - dragon-drag-and-drop
             - curl


Usage: $0 files|--files|-f
       $0 urls|--urls|-u
       $0 help|--help|-h


The following specific options are supported:

  -f, --files       show drop dialog for files - copy them to current directory
  -u, --urls        show drop dialog for (image) urls - download them into current directory
  -h, --help        display this help
EOF
    exit $1
}


filesDrop() {
    IFS=$'\n'
    for src in $(dragon-drag-and-drop -t -x -p); do 
        dest="$PWD/$(basename $src)"
        echo "cp \"$src\" \"$dest\""
        eval "cp \"$src\" \"$dest\""
    done
    unset IFS

    echo "copy completed"
    exit
}


urlsDrop() {
    IFS=$'\n'
    for src in $(dragon-drag-and-drop -t -x); do
        if grep "^https://" <<< "$src" >/dev/null; then
            dest="$PWD/$(basename $src)"
            echo "download to $dest"
            [ -f $dest ] || curl -q -o "$dest" "$src" >/dev/null 2>&1
        fi
    done
    unset IFS

    echo "download completed"
    exit
}


# MAIN
case "$1" in
    files|--files|-f)           shift; filesDrop ;;
    urls|--urls|-u)             shift; urlsDrop ;;
    help|--help|-h)             usage 0 ;;
    *)                          usage 1 ;;
esac

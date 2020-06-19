#!/usr/bin/env zsh

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""
[ -z "$TERMINAL" ] && notify-send "Error" "Environment variable \$TERMINAL is not set" && exit 1
[ -z "$EDITOR" ] && notify-send "Error" "Environment variable \$EDITOR is not set" && exit 1
[ ! -d $HOME/.dotfiles ] && notify-send "Error" "Dotfiles are not stored in a git bare repository" && exit 1

choice=$(git --git-dir=$HOME/.dotfiles --work-tree=$HOME ls-tree -r master --name-only --full-name | \
    xargs -I{} echo "$HOME/{}" | sort | eval "dmenu -i -l 15 -p \"Edit Config >\" $DMENU_STYLE") || exit

[ -e $choice ] || exit
case $(file --mime-type $choice -b) in
    image/*) setsid sxiv $choice >/dev/null 2>&1 & ;;
    application/pdf) setsid $READER $fx >/dev/null 2>&1 & ;;
    # NOTE: In some Terminals nvim does not resize if called direct, workaround add sleep
    text/*|*) $TERMINAL -e sh -c "sleep 0.1; $EDITOR $choice;" ;;
esac


# zsh functions

# function to print a color palette
color-palette() {
    local -a colors
    for i in {000..255}; do
        colors+=("%F{$i}$i%f")
    done
    print -cP $colors
}

# function to open a new terminal in current directory (working directory split)
wd-split() {
    [ -v TMUX ] || TMUX=""
    if [ -n "$TMUX" ]; then
        tmux new-window
    else
        setsid $TERMINAL >/dev/null 2>&1
    fi
}

# run this function to fix "unknown terminal type" error in ssh sessions
ssh-terminfo() {
    [ -z "$1" ] && echo "usage: ssh-terminfo -p [PORT] [USER@HOST]" && return
    infocmp $TERM | eval "ssh $@ \"mkdir -p ~/.terminfo && cat >/tmp/ti && tic /tmp/ti\""
    # other solution: alias ssh="TERM=linux ssh"
}

# update pacman mirrorlist with reflector
mirrorlist-update() {
   sudo cp -f /etc/pacman.d/mirrorlist /etc/pacman.d/mirrorlist.bak
   sudo reflector --verbose --protocol https --country "Germany" --latest 30 --sort rate --save /etc/pacman.d/mirrorlist
}

# mkdir & cd to it
function md() { 
  mkdir -p "$1" && cd "$1"; 
}

# search in history
function hs() {
    history | grep $*
}

# remove a git submodule
function git-submodule-remove() {
    [ ! -d .git ] && echo "no git root directory" && return
    [ ! -f .gitmodules ] && echo ".gitmodules not exist" && return
    submodulPath="$@"
    [ ! -d "$submodulPath" ] && echo "submodul path not exist" && return
    grep "path = $submodulPath" .gitmodules >/dev/null || return
    git submodule deinit -f "$submodulPath"
    rm -rf ".git/modules/${submodulPath}"
    git rm -f "$submodulPath"
    echo -e ">> The submodule was successfully removed.\nNote: the changes still have to be committed."
}


# zsh alias

alias zsh-vim="export ZSH_KEYMAP=\"vim\"; source $ZDOTDIR/.zshrc"
alias zsh-emacs="export ZSH_KEYMAP=\"emacs\"; source $ZDOTDIR/.zshrc"
alias vim="nvim" vimdiff="nvim -d" vi="nvim" v="nvim"
alias ls="lsd" l="lsd -l" la="lsd -la" lt="lsd -Rl --depth 2" l.='ls -a | egrep "^\." | grep -vE "^(\.|\.\.)$"'
alias dolphin="dolphin -stylesheet $HOME/.config/qt5ct/qss/dolphin.qss"
alias tmux='tmux -f $XDG_CONFIG_HOME/tmux/tmux.conf' t='tmux' fm='tmux new-session lf'
alias mi="mediainfo"
alias netstat="netstat --wide"
alias yay="yay --sudoloop"
alias calc="qalc"
alias s="sxiv"
alias z="zathura"
alias yt-dl-audio="youtube-dl -x --audio-format mp3"
alias watch-dir="inotifywait -e modify,create -r $PWD -m"
alias nmap="grc nmap"  # colorize nmap output
alias pacman-unlock="sudo rm /var/lib/pacman/db.lck"
alias grep="grep --color=auto" egrep="egrep --color=auto" fgrep="fgrep --color=auto"
alias df="df -h"
alias free="free -m"
alias pacman-fzf="pacman -Slq | fzf --multi --preview 'pacman -Si {1}' | xargs -ro sudo pacman -S"
alias grep-todo='grep --color=auto -r --exclude-dir="\.git" -i -n "TODO" .'
alias gw-ping-tcp="sudo nping --tcp --dest-mac \$(arp | grep "gateway" | sed 's/ * / /g' | cut -d ' ' -f 3) " # [IP] -p [Port]
alias gw-ping-icmp="sudo nping --icmp --dest-mac \$(arp | grep "gateway" | sed 's/ * / /g' | cut -d ' ' -f 3) " # [IP]
alias mpv-vr="mpv --script=~/.config/mpv/src/vr-reversal/360plugin.lua"
alias video-concat-all="ffmpeg -safe 0 -f concat -i <(find . -type f -maxdepth 1 -name '*' -printf \"file '\$PWD/%p'\n\" | sort) -c copy concat.mkv"
alias :q="exit" :Q="exit"
alias h="history -30" clh="[ -f \$HISTFILE ] && echo '' > \$HISTFILE && exec \$SHELL -l"
alias ..="cd .." ...="cd ../.." ....="cd ../../.." .....="cd ../../../.." ......="cd ../../../../.."
alias .1="cd .." .2="cd ../.." .3="cd ../../.." .4="cd ../../../.." .5="cd ../../../../.."
alias ~="cd ~"
alias cp="cp -rv"
alias del="trash-put"
alias mv='mv -v'
alias py="python3"
alias cal="cal -m -3 --color=always"
alias cl="clear"  #NOTE: use [Ctrl+l]

# web alias
alias public-ip="curl http://ipecho.net/plain; echo"
alias weather="curl wttr.in/waiblingen"
alias weather-details="curl v2.wttr.in/waiblingen"
alias coin-rate="curl rate.sx"

# git alias
# forgit zsh plugin (see: https://github.com/wfxr/forgit):
# ga (git add selector), glo (git log viewer), gi (gitignore generator), gd (git diff viewer), grh (gir reset HEAD <file> selector), \
# gcf (git checkout <file> selector), gss (git stash viewer), gclean (git clean selector)
alias g="git" gs="git status" gb="git branch" gcmsg="git commit -m" gco="git checkout" gf="git fetch" gl="git pull && git submodule update" \
    gls="git ls-tree -r --name-only HEAD | sort" gm="git merge" gp="git pull && git push" greset="git reset --hard HEAD && git clean -f -d" git-edit-last-commit="git commit --amend"

alias add="git add" branch="git branch" commit="git commit -m" pull="git pull && git submodule update" push="git push" status="git status"

# btrfs
alias btrfs="sudo btrfs"
alias btrfs-recover="dd conv=noerror,sync"  # use btrfs-recover if=corruptFile of=destFileAtOtherDrive
alias btrfs-csum-check="sudo btrfs check --check-data-csum --force"  # use btrfs-csum-check /dev/mapper/xxx
alias btrfs-file-by-bytenr="btrfs inspect-internal logical-resolve"  # use btrfs-file-by-bytenr <bytenr> <path>

# dotfiles bare git repository
if [ -f $HOME/.dotfiles/config ]; then
    alias dotfiles='git --git-dir=$HOME/.dotfiles --work-tree=$HOME'
    alias dotfiles-status='dotfiles status -v'
    alias dotfiles-add='dotfiles add -v'
    alias dotfiles-commit='dotfiles add -u && dotfiles commit -m'
    alias dotfiles-ls='dotfiles ls-files --full-name | xargs -I{} echo "$HOME/{}" | sort'
    alias dotfiles-edit='dotfiles-ls | fzf --reverse | xargs -I{} $EDITOR "{}"'
    alias dotfiles-update='dotfiles pull && dotfiles submodule update --remote && echo "update completed"'
    alias dotfiles-reset='dotfiles reset --hard HEAD && dotfiles pull; dotfiles submodule update --remote'
    alias dotfiles-search='dotfiles-ls | xargs -I{} echo "\"{}\"" | xargs grep --color=auto -n -s -H -i'
    alias dotfiles-todo='dotfiles-search TODO'
    alias .aa="dotfiles add -u -v"
    alias .cmsg='dotfiles commit -m'
    alias .p='dotfiles push'
    alias .s='dotfiles status -s'
    alias .ls="dotfiles-ls"

    # dotfiles add-commit-push function
    function .acp() {
        _dotfiles_command='git --git-dir=$HOME/.dotfiles --work-tree=$HOME'
        [ "$(eval "$_dotfiles_command status -s | wc -l")" -le "0" ] && echo "[INFO] There is nothing to commit" && return
        eval "$_dotfiles_command status -s"
        if [ -z "$1" ]; then
            echo -en "commit message: " && read msg
        else
            msg="$@"
            echo "commit message: $msg"
        fi
        [ -z "$msg" ] && msg="update dotfiles"
        echo -en "execute process? [Y/n]" && read choice
        if [ "$choice" = "N" ] || [ "$choice" = "n" ] || [ "$choice" = "no" ] || [ "$choice" = "no" ]; then
            echo "> cancel" && return
        fi
        eval "$_dotfiles_command add -u -v"
        eval "$_dotfiles_command commit -m \"$msg\""
        eval "$_dotfiles_command push"
    }
fi

# x11 tools alias
if [[ "$XDG_SESSION_TYPE" = "x11" ]]; then
    [ -f ~/.local/bin/x11-devour ] && alias sw="x11-devour"
    alias xinfo="xprop WM_NAME WM_CLASS"
    alias xcolor="xcolor | tr -d '\n' | xclip -selection clipboard"
    alias xclip-oneline="xclip -o -selection clipboard | tr '\n' ' ' | tr -dc '[:print:]' | xclip -i -selection clipboard"
    alias xclip-send='echo "Send clipboard via virtual keypress. Start in 4 sec (use Ctrl+C to abort)"; sleep 4; setxkbmap de; xdotool type --clearmodifiers "$(xsel -b)"'
fi



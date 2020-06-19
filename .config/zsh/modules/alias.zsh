# zsh alias

# alias if program is present
command -v nvim >/dev/null && alias vim="nvim" vimdiff="nvim -d" vi="nvim" v="nvim"
command -v lsd >/dev/null && alias ls="lsd" l="lsd -l" la="lsd -la" lr="lsd -Rl --depth 2"
command -v trash-put >/dev/null && alias trm="trash-put"
command -v dolphin >/dev/null && alias dolphin="dolphin -stylesheet $HOME/.config/qt5ct/qss/dolphin.qss"
command -v tmux >/dev/null && alias tmux='tmux -f $XDG_CONFIG_HOME/tmux/tmux.conf' t='tmux' fm='tmux new-session lf'
command -v mediainfo >/dev/null && alias mi="mediainfo"
command -v netstat >/dev/null && alias netstat="netstat --wide"
command -v yay >/dev/null && alias yay="yay --sudoloop"
command -v qalc >/dev/null && alias calc="qalc"
command -v sxiv >/dev/null && alias s="sxiv"
command -v zathura >/dev/null && alias z="zathura"

# base command alias
alias :q="exit" :Q="exit"
alias h="history" clh="[ -f \$HISTFILE ] && echo '' > \$HISTFILE && exec \$SHELL -l"
alias ..="cd .." ...="cd ../.." ....="cd ../../.." .....="cd ../../../.." ~="cd ~"
alias cp="cp -rv"
setopt rm_star_silent  # disable the double verification in zsh
alias rm="rm -vrfI"
alias mv='mv -v'
alias py="python"
alias cal="cal -m -3 --color=always"
alias cl="clear"  #NOTE: use [Ctrl+l]

# web alias
alias public-ip="curl http://ipecho.net/plain; echo"
alias weather="curl wttr.in"
alias coin-market="curl rate.sx"

# git alias
# forgit zsh plugin (see: https://github.com/wfxr/forgit):
# ga (git add selector), glo (git log viewer), gi (gitignore generator), gd (git diff viewer), grh (gir reset HEAD <file> selector), \ 
# gcf (git checkout <file> selector), gss (git stash viewer), gclean (git clean selector)
alias g="git" gs="git status" gaa="git add --all" gb="git branch" gcmsg="git commit -m" gco="git checkout" gf="git fetch" \
    gl="git pull && git submodule update" gm="git merge" gp="git pull && git push" greset="git reset --hard HEAD && git clean -f -d"

# auto-sudo
alias btrfs="sudo btrfs"

# Dotfiles bare git repository (Use 'dotfiles add -u' or 'dotfiles add -f <file>' to stage changes)
if [ -f $HOME/.dotfiles/config ]; then
    alias dotfiles='git --git-dir=$HOME/.dotfiles --work-tree=$HOME' 
    alias dotfiles-commit='dotfiles add -u && dotfiles commit -m'
    alias dotfiles-ls='dotfiles ls-tree -r HEAD --name-only --full-name | xargs -I{} echo "$HOME/{}" | sort'
    alias dotfiles-edit='dotfiles-ls | fzf --reverse | xargs -I{} $EDITOR "{}"'
    alias dotfiles-update='dotfiles reset --hard HEAD && dotfiles pull && dotfiles submodule update && echo "update completed"'
    alias dotfiles-search='dotfiles-ls | xargs -I{} echo "\"{}\"" | xargs grep -n -s -H -i'
fi

# x11 tools alias
if [[ "$XDG_SESSION_TYPE" = "x11" ]]; then
    command -v xprop >/dev/null && alias xinfo="xprop WM_NAME WM_CLASS"
    command -v xcolor >/dev/null && alias xcolor="xcolor | tr -d '\n' | xclip -selection clipboard"
    command -v xclip >/dev/null && alias xclip-oneline="xclip -o -selection clipboard | tr '\n' ' ' | tr -dc '[:print:]' | xclip -i -selection clipboard"
    command -v xdotool >/dev/null && alias xclip-send='echo "Send clipboard via virtual keypress. Start in 4 sec (use Ctrl+C to abort)"; sleep 4; setxkbmap de; xdotool type --clearmodifiers "$(xsel -b)"'
fi

# i3 alias
if echo "$DESKTOP_SESSION" | grep "i3" >/dev/null; then
    [ -f $HOME/.config/i3/scripts/i3-swallow ] && alias sw="$HOME/.config/i3/scripts/i3-swallow"
fi

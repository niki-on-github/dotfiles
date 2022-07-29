# zsh bindings

#################################################################################################################
# terminal mode (vim or emacs)
#################################################################################################################

vim_ins_mode="[INS]"
vim_cmd_mode="[CMD]"
vim_mode=$vim_ins_mode

# Call this function with "vim" or "emacs"
setup_mode() {
    mode="emacs" # default mode is emacs
    [ -n "$1" ] && [ "$1" = "vim" ] && mode="vim"

    if [ "$mode" = "vim" ]; then
        bindkey -v
        export KEYTIMEOUT=1 # Reduce esc delay in vim mode
    else
        bindkey -e
    fi

    # Make sure that the terminal is in application mode when zle is active (required for some bindings)
    if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
        function zle-line-init() {
            echoti smkx
            [ "$mode" = "vim" ] && zle -K viins # Start every prompt in insert mode
        }
        function zle-line-finish() {
            echoti rmkx
            vim_mode=$vim_ins_mode
        }
        zle -N zle-line-init
        zle -N zle-line-finish
    fi

    if [ "$mode" = "vim" ]; then
        function zle-keymap-select {
            case $KEYMAP in
                vicmd) vim_mode=${vim_cmd_mode} ;;
                viins|main) vim_mode=${vim_ins_mode} ;;
            esac
            zle reset-prompt
        }
        zle -N zle-keymap-select

        function TRAPINT() {
            vim_mode=$vim_ins_mode
            return $(( 128 + $1 ))
        }
    fi
}

# setup mode before assing bindings
setup_mode "$ZSH_KEYMAP"


#################################################################################################################
# binding functions
#################################################################################################################

# use lf to switch directories
function lf-cd {
    tmp="$(mktemp)"
    lf -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp"
        if [ -d "$dir" ]; then
            if [ "$dir" != "$(pwd)" ]; then
                cd "$dir"
            fi
        fi
    fi
    zle reset-prompt
}
zle -N lf-cd

# retrieves and runs last command
function run-again {
    zle up-history # get previous history item
    zle accept-line # confirm command
}
zle -N run-again

# add string to zsh history
function zshaddhistory() {
    print -sr -- ${1%%$'\n'}
}

# toggel vim, emacs zsh keymap
function toggle-zsh-keymap {
    if [ "$ZSH_KEYMAP" = "emacs" ]; then
        export ZSH_KEYMAP="vim"
    else
        export ZSH_KEYMAP="emacs"
    fi
    zshaddhistory "$BUFFER"
    source $ZDOTDIR/.zshrc
    zle kill-whole-line
    zle accept-line
}
zle -N toggle-zsh-keymap

# toggles "sudo/sudoedit" before the current/previous command
function sudo-command-line() {
    [[ -z $BUFFER ]] && zle up-history
    if [[ $BUFFER == sudo\ * ]]; then
        LBUFFER="${LBUFFER#sudo }"
    elif [[ $BUFFER == $EDITOR\ * ]]; then
        LBUFFER="${LBUFFER#$EDITOR }"
        LBUFFER="sudoedit $LBUFFER"
    elif [[ $BUFFER == v\ * ]]; then
        LBUFFER="${LBUFFER#v }"
        LBUFFER="sudoedit $LBUFFER"
    elif [[ $BUFFER == sudoedit\ * ]]; then
        LBUFFER="${LBUFFER#sudoedit }"
        LBUFFER="$EDITOR $LBUFFER"
    else
        LBUFFER="sudo $LBUFFER"
    fi
    zle end-of-line
}
zle -N sudo-command-line

# paste from system clipboard
if [ "$XDG_SESSION_TYPE" = "wayland" ]; then
    paste-clip() {
        killring=("$CUTBUFFER" "${(@)killring[1,-2]}")
        CUTBUFFER=$(wl-paste)
        zle yank
    }
else
    paste-clip() {
        killring=("$CUTBUFFER" "${(@)killring[1,-2]}")
        CUTBUFFER=$(xclip -selection clipboard -o 2>/dev/null)
        if [ "$?" != "0" ]; then
            if command -v notify-send >/dev/null ; then
                notify-send "Clipboard" "Error: target string not available"
            else
                echo "Clipboard Error: Target string not available"
            fi
        fi
        zle yank
    }
fi
zle -N paste-clip

# print shortcuts list to improve your zsh productivity
function bindings {
  echo "\
  ALT  + D              Delete the word after the cursor
  ALT  + [Backspace]    Delete word backward
  CTRL + A              Move to the beginning of the line
  CTRL + E              Move to the end of the line
  CTRL + [Left Arrow]   Move one word backward
  CTRL + [Right Arrow]  Move one word forward
  ESC  + B              Move one word backward
  ESC  + F              Move one word forward
  CTRL + U              Clear the entire line
  CTRL + K              Clear the characters on the line after the current cursor position
  CTRL + W              Delete the word in front of the cursor
  CTRL + R              Search history
  CTRL + G              Escape from search mode
  CTRL + _              Undo the last change
  CTRL + L              Clear screen
  CTRL + C              Terminate/kill current foreground process
  CTRL + Z              Suspend/stop current foreground process
  CTRL + [Space]        Auto Complete command and execute
  CTRL + M              Execute current command
  ESC  + [Backspace]    Delete the word in front of the cursor"
  # CTRL + S              Stop output to screen
  # CTRL + Q              Re-enable screen output
}


#################################################################################################################
# key bindings (use `sed -n l` to get the key code)
#################################################################################################################

# function keys
[[ "${terminfo[khome]}" != "" ]]    && bindkey "${terminfo[khome]}" beginning-of-line       # [Home] - Go to beginning of line
[[ "${terminfo[kend]}" != "" ]]     && bindkey "${terminfo[kend]}" end-of-line              # [End] - Go to end of line
[[ "${terminfo[kdch1]}" != "" ]]    && bindkey "${terminfo[kdch1]}" delete-char             # [Backspace] - delete backward
[[ "${terminfo[kpp]}" != "" ]]      && bindkey "${terminfo[kpp]}" up-line-or-history        # [PageUp] - Up a line of history
[[ "${terminfo[knp]}" != "" ]]      && bindkey "${terminfo[knp]}" down-line-or-history      # [PageDown] - Down a line of history
[[ "${terminfo[kcbt]}" != "" ]]     && bindkey "${terminfo[kcbt]}" reverse-menu-complete    # [Shift-Tab] - move through the completion menu backwards
[[ "${terminfo[kdch1]}" != "" ]]    && bindkey "${terminfo[kdch1]}" delete-char             # [Delete] - delete forward

bindkey ' ' magic-space                             # [Space] - do history expansion
bindkey '^?' backward-delete-char                   # [Backspace] - delete backward
bindkey '^[[1;5C' forward-word                      # [Ctrl+RightArrow] - move forward one word
bindkey '^[[1;5D' backward-word                     # [Ctrl+LeftArrow] - move backward one word
bindkey '^r' history-incremental-search-backward    # [Ctrl+r] - Search backward incrementally for a specified string
bindkey '^ ' autosuggest-execute                    # [Ctrl+Space] - auto complete command and incrementaly execute command
bindkey '^x' toggle-zsh-keymap                      # [Ctrl+x] - toggle vim, emacs zsh keymap
bindkey '^o' lf-cd                                  # [Ctrl+o] - use lf to switch directory
bindkey -r '^V'; bindkey "^V" paste-clip            # [Ctrl+v] - paste from system clipboard (vi-quoted-insert conflict with Paste)
bindkey -r '^Y'                                     # [Ctrl+y] - conflict with Copy (alacritty)
# bindkey '^P' fuzzy-search-and-edit                  # seletskiy/zsh-fuzzy-search-and-edit plugin
# bindkey "\e\e" sudo-command-line                    # [ESC][ESC] toggles "sudo/sudoedit" before the current/previous command
bindkey '^M' accept-line

# fix tmux pos1 and ende key
bindkey "\E[1~" beginning-of-line
bindkey "\E[4~" end-of-line

# Run manpage on Esc+h
autoload -Uz run-help
autoload -Uz run-help-git
autoload -Uz run-help-sudo
bindkey '^[h' run-help  # Esc+h

# start typing + [Up-Arrow] - fuzzy find history forward
if [[ "${terminfo[kcuu1]}" != "" ]]; then
    autoload -U up-line-or-beginning-search
    zle -N up-line-or-beginning-search
    bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search
    bindkey '^p' up-line-or-beginning-search
fi

# start typing + [Down-Arrow] - fuzzy find history backward
if [[ "${terminfo[kcud1]}" != "" ]]; then
    autoload -U down-line-or-beginning-search
    zle -N down-line-or-beginning-search
    bindkey "${terminfo[kcud1]}" down-line-or-beginning-search
    bindkey '^n' down-line-or-beginning-search
fi

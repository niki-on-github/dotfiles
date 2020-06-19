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
setup_mode "emacs"


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

# close if terminal run inside tmux as lf-termina
function close {
    [[ -v TMUX ]] || TMUX=""
    [ -n "$TMUX" ] && tmux lsp -F "#{pane_title}" | grep "^lf-terminal$" >/dev/null && exit
}
zle -N close

# retrieves and runs last command
function run-again {
    zle up-history # get previous history item
    zle accept-line # confirm command
}
zle -N run-again


#################################################################################################################
# key bindings (use `sed -n l` to get the key code)
#################################################################################################################

# non zsh specific
# Ctrl + U      delete from the cursor to the start of the line.
# Ctrl + K      delete from the cursor to the end of the line.
# Ctrl + W      delete from the cursor to the start of the preceding word.
# Ctrl + L      clear the terminal.

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
bindkey '^[[1;5C' forward-word                      # [Ctrl-RightArrow] - move forward one word
bindkey '^[[1;5D' backward-word                     # [Ctrl-LeftArrow] - move backward one word
bindkey '^r' history-incremental-search-backward    # [Ctrl-r] - Search backward incrementally for a specified string
bindkey '^ ' autosuggest-accept                     # [Ctrl+Space] - auto complete command
bindkey '^x' run-again                              # [Ctrl+x] - run last command again
bindkey '^o' lf-cd                                  # [Ctrl+o] - use lf to switch directory
bindkey '^[OS' close                                # [F4] - close if terminal run inside tmux

# start typing + [Up-Arrow] - fuzzy find history forward
if [[ "${terminfo[kcuu1]}" != "" ]]; then
    autoload -U up-line-or-beginning-search
    zle -N up-line-or-beginning-search
    bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search
fi

# start typing + [Down-Arrow] - fuzzy find history backward
if [[ "${terminfo[kcud1]}" != "" ]]; then
    autoload -U down-line-or-beginning-search
    zle -N down-line-or-beginning-search
    bindkey "${terminfo[kcud1]}" down-line-or-beginning-search
fi

# remove bindings
bindkey -r '^V'  # vi-quoted-insert conflict with 'Ctrl+V' (Paste)

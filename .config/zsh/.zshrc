# ZSH config (main)
# NOTE: This file also works from the user home directory if ZDOTDIR environmentvariable is not set. You can copy this file into the user home directory and everything will work properly.

#################################################################################################################
# zsh plugin manager
#################################################################################################################

zsh-plugins() {
    # auto install zinit
    if [ ! -f ~/.config/zinit/bin/zinit.zsh ]; then
        mkdir -p ~/.config/zinit
        git clone https://github.com/zdharma/zinit.git ~/.config/zinit/bin
    fi

    # init zinit
    source ~/.config/zinit/bin/zinit.zsh
    autoload -Uz _zinit
    (( ${+_comps} )) && _comps[zinit]=_zinit

    # set default zsh keymap
    [ -n "$ZSH_KEYMAP" ] || export ZSH_KEYMAP="emacs"

    # my zsh functions
    fpath+=( $HOME/.config/zsh/functions )
    for f in $HOME/.config/zsh/functions/* ; do
        autoload -Uz $f
    done

    # Git fzf wrapper
    zinit light wfxr/forgit

    # History substring searching
    zinit light zsh-users/zsh-history-substring-search

    # Autosuggestions
    zinit ice wait lucid atload'_zsh_autosuggest_start'
    zinit light zsh-users/zsh-autosuggestions
    export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20

    # Tab completions
    zinit ice wait lucid blockf atpull'zinit creinstall -q .'
    zinit light zsh-users/zsh-completions

    # Syntax highlighting
    # TODO: enable asynchronous loading once issue https://github.com/zdharma/fast-syntax-highlighting/issues/177 is resolved (uncomment line below)
    #zinit ice wait lucid atinit'zpcompinit; zpcdreplay'
    zinit light zdharma/fast-syntax-highlighting

    # synchronize zsh with system clipboard
    if [ "$ZSH_KEYMAP" = "vim" ]; then
        zinit light kutsan/zsh-system-clipboard
        typeset -g ZSH_SYSTEM_CLIPBOARD_TMUX_SUPPORT='true'
    fi

    # my zsh modules
    for f in $HOME/.config/zsh/modules/*.zsh ; do
        source $f
    done

    # fzf zab completions (must be last plugin)
    source $HOME/.config/zsh/plugin-configs/fzf-tab.zsh
    zinit ice wait lucid atinit'zpcompinit; zpcdreplay'
    zinit light Aloxaf/fzf-tab

    # zsh theme
    setopt PROMPT_SUBST
    if echo "${$(tty):5}" | grep "^tty" >/dev/null ; then  # tty
        PROMPT='%F{blue}%/%f > '
    elif [ -v DISPLAY ]; then
        if grep "localhost" <<< $DISPLAY >/dev/null ; then # x11 forwarding
            PROMPT='%F{blue}%/%f > '
        else # graphical environment
            zinit light "$HOME/.config/zsh/theme"
        fi
    else  # ssh
        PROMPT='%F{blue}%/%f > '
    fi
}


#################################################################################################################
# MAIN
#################################################################################################################

zmodload zsh/zprof  # use zprof to profiling zsh
zsh-plugins
# [ $? = 0 ] && clear

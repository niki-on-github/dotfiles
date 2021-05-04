# ZSH config (main)
# NOTE: This file also works from the user home directory if ZDOTDIR environmentvariable is not set. You can copy this file into the user home directory and everything will work properly.

#################################################################################################################
# zsh plugin manager (Update with `zplug update`)
#################################################################################################################

zsh-plugins() {
    # auto install zinit
    if [ ! -f ~/.config/zinit/bin/zinit.zsh ]; then
        mkdir -p ~/.config/zinit
        git clone https://github.com/zdharma/zinit.git ~/.config/zinit/bin
    fi

    ZSH_DISPLAY='graphical-environment'
    if echo "${$(tty):5}" | grep -q "^tty" ; then
        ZSH_DISPLAY='tty'
    elif [ -v DISPLAY ]; then
        if grep -q "localhost" <<< $DISPLAY ; then
            ZSH_DISPLAY='x11-forwarding'
        else
            ZSH_DISPLAY='graphical-environment'
        fi
    else
        ZSH_DISPLAY='ssh'
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

    # z, autojump command
    zinit ice wait blockf lucid
    zinit light rupa/z

    # z tab completion, lets z+[Tab] and zz+[Tab]
    zinit ice wait lucid
    zinit light changyuheng/fz

    # z / fzf, Pick from most frecent folders with `Ctrl+g`
    if [ "$ZSH_DISPLAY" = 'graphical-environment' ] || [ "$ZSH_DISPLAY" = 'x11-forwarding' ] || [ "$ZSH_DISPLAY" = 'ssh' ]; then
        zinit ice wait lucid
        zinit light andrewferrier/fzf-z
    fi

    # Fuzzy search all text by `Ctrl+P` in a file and open line in `$EDITOR`
    if [ "$ZSH_DISPLAY" = 'graphical-environment' ] || [ "$ZSH_DISPLAY" = 'x11-forwarding' ] || [ "$ZSH_DISPLAY" = 'ssh' ]; then
        zinit load mafredri/zsh-async
        zinit load seletskiy/zsh-fuzzy-search-and-edit
    fi

    # Git fzf wrapper
    if [ "$ZSH_DISPLAY" = 'graphical-environment' ] || [ "$ZSH_DISPLAY" = 'x11-forwarding' ] || [ "$ZSH_DISPLAY" = 'ssh' ]; then
        zinit light wfxr/forgit
    fi

    # History substring searching
    zinit light zsh-users/zsh-history-substring-search

    # Autosuggestions
    if [ "$ZSH_DISPLAY" = 'graphical-environment' ] || [ "$ZSH_DISPLAY" = 'x11-forwarding' ] || [ "$ZSH_DISPLAY" = 'ssh' ]; then
        zinit ice wait lucid atload'_zsh_autosuggest_start'
        zinit light zsh-users/zsh-autosuggestions
        export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
    fi

    # Tab completions
    zinit ice wait lucid blockf atpull'zinit creinstall -q .'
    zinit light zsh-users/zsh-completions

    # Syntax highlighting
    if [ "$ZSH_DISPLAY" = 'graphical-environment' ] || [ "$ZSH_DISPLAY" = 'x11-forwarding' ] || [ "$ZSH_DISPLAY" = 'ssh' ]; then
        # TODO: enable asynchronous loading once issue https://github.com/zdharma/fast-syntax-highlighting/issues/177 is resolved (uncomment line below)
        #zinit ice wait lucid atinit'zpcompinit; zpcdreplay'
        zinit light zdharma/fast-syntax-highlighting
    fi

    if [ "$ZSH_DISPLAY" = 'graphical-environment' ] ; then
        # synchronize zsh with system clipboard
        if [ "$ZSH_KEYMAP" = "vim" ]; then
            zinit light kutsan/zsh-system-clipboard
            typeset -g ZSH_SYSTEM_CLIPBOARD_TMUX_SUPPORT='true'
        fi
    fi

    # my zsh modules
    for f in $HOME/.config/zsh/modules/*.zsh ; do
        source $f
    done

    # fzf zab completions (must be last plugin)
    if [ "$ZSH_DISPLAY" = 'graphical-environment' ] || [ "$ZSH_DISPLAY" = 'x11-forwarding' ] || [ "$ZSH_DISPLAY" = 'ssh' ]; then
        zinit ice wait lucid atinit'zpcompinit; zpcdreplay'
        zinit light Aloxaf/fzf-tab
    fi

    # zsh theme
    setopt PROMPT_SUBST
    if [ "$ZSH_DISPLAY" = 'graphical-environment' ] ; then
        if [ "$TERM" = 'alacritty' ] || [ "$TERM" = 'tmux-256color' ]; then
            zinit light "$HOME/.config/zsh/theme"
        else
            zinit ice compile'(pure|async).zsh' pick'async.zsh' src'pure.zsh'
            zinit light sindresorhus/pure
        fi
    elif [ "$ZSH_DISPLAY" = 'x11-forwarding' ] ; then
        zinit light "$HOME/.config/zsh/theme"
    elif [ "$ZSH_DISPLAY" = 'ssh' ] ; then
        zinit light "$HOME/.config/zsh/theme"
    elif [ "$ZSH_DISPLAY" = 'tty' ] ; then
        PROMPT='%F{blue}%/%f > '
    else
        PROMPT='%F{blue}%/%f > '
    fi
}


#################################################################################################################
# MAIN
#################################################################################################################

zmodload zsh/zprof  # use zprof to profiling zsh
zsh-plugins

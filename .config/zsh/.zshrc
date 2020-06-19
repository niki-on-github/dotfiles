# ZSH config (main)

#################################################################################################################
# zsh plugins (use 'zplug update' to update plugins)
#################################################################################################################

ZPlug() {
    export ZPLUG_HOME=$HOME/.config/zplug

    if [ ! -d $ZPLUG_HOME ]; then
        curl -sL --proto-redir -all,https \
            https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
        echo "setup ..." && sleep 5  # sleep required to complete setup 
    fi

    # init zplug
    source $ZPLUG_HOME/init.zsh

    # zsh plugins
    zplug "wfxr/forgit"
    zplug "urbainvaes/fzf-marks"
    zplug "pierpo/fzf-docker"
    zplug "zsh-users/zsh-autosuggestions", defer:1 
    zplug "zsh-users/zsh-syntax-highlighting", defer:2
    zplug "zsh-users/zsh-history-substring-search", defer:3
    zplug "Aloxaf/fzf-tab", defer:3  # call other plugins which are bound to "^I" (if init last)  

    # local plugins
    zplug "$ZDOTDIR/modules", from:local, defer:2

    # zsh theme
    zplug "$ZDOTDIR/theme", from:local, as:theme

    # Install new plugins
    if ! zplug check --verbose; then
        zplug install
    fi

    zplug load
}


#################################################################################################################
# MAIN
#################################################################################################################

if [ -v DISPLAY ]; then
    # graphical environment
    ZPlug
else
    # tty
    PROMPT='%F{blue}%/%f > '
fi

[ $? = 0 ] && clear

# zsh anaconda

# anaconda delayed loading
function import-anaconda() {
    if grep "anaconda3" <<< $PATH >/dev/null; then; return; fi
    if [ -d $HOME/anaconda3 ]; then
        unalias conda  # ovveride the first call alias
        __conda_setup="$('$HOME/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
        if [ $? -eq 0 ]; then
            eval "$__conda_setup"
        else
            if [ -f "$HOME/anaconda3/etc/profile.d/conda.sh" ]; then
                . "$HOME/anaconda3/etc/profile.d/conda.sh"
            else
                export PATH="$HOME/anaconda3/bin:$PATH"
            fi
        fi
        unset __conda_setup
    fi
    # NOTE: Disable the automatic base activation: `conda config --set auto_activate_base false`
}
alias conda="import-anaconda; conda"  # first call in new shell import anaconda

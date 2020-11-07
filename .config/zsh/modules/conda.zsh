# zsh conda
# NOTE: Disable the automatic base activation with: `conda config --set auto_activate_base false`

# miniconda delayed loading
function import-miniconda() {
    if grep -q "miniconda3" <<< $PATH; then; return; fi
    if [ -d $HOME/miniconda3 ]; then
        unalias conda  # ovveride the first call alias
        __conda_setup="$('$HOME/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
        if [ $? -eq 0 ]; then
            eval "$__conda_setup"
        else
            if [ -f "$HOME/miniconda3/etc/profile.d/conda.sh" ]; then
                . "$HOME/miniconda3/etc/profile.d/conda.sh"
            else
                export PATH="$HOME/miniconda3/bin:$PATH"
            fi
        fi
        unset __conda_setup
    fi
}

# anaconda delayed loading
function import-anaconda() {
    if grep -q "anaconda3" <<< $PATH; then; return; fi
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
}

if [ -d $HOME/miniconda3 ]; then
    alias conda="import-miniconda; conda"
elif [ -d $HOME/anaconda3 ]; then
    alias conda="import-anaconda; conda"
fi

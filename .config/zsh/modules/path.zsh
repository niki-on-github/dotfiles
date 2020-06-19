# zsh path

# import anaconda
# NOTE: Disable the automatic base activation: `conda config --set auto_activate_base false`
# Jupyther Notebooks: naviagate to the directory with jupyter notebook, activate a conda environemnt and type `jupyther-notebook`
if [ -d $HOME/anaconda3 ]; then
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

# add '~/.local/bin' to PATH
[ -d $HOME/.local/bin ] && path+=$HOME/.local/bin

# LaTeX vim live preview
[ -d $HOME/.config/nvim/plugged/vim-live-latex-preview/bin ] && \
    path+=$HOME/.config/nvim/plugged/vim-live-latex-preview/bin

# npm without sudo
npm set prefix ~/.npm
path+=$HOME/.npm/bin
path+=./node_modules/.bin


[ "path" ]  # set exit code to 0

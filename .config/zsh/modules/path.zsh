# zsh path

# add '~/.local/bin' to PATH
path+=$HOME/.local/bin

# LaTeX vim live preview
# [ -d $HOME/.config/nvim/plugged/vim-live-latex-preview/bin ] && \
#     path+=$HOME/.config/nvim/plugged/vim-live-latex-preview/bin

# npm without sudo
#npm set prefix ~/.npm  # slowdown zsh start
path+=$HOME/.npm/bin
path+=./node_modules/.bin

[ "path" ]  # set exit code to 0

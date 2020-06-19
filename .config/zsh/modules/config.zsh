# zsh config

# load colors
autoload -U colors && colors

# Automatically change directory if a directory is entered
setopt autocd
setopt extendedglob

# Smart URLs
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

# zsh config

# Automatically change directory if a directory is entered
setopt autocd
setopt extendedglob

# Smart URLs
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

# disable the double verification in zsh
setopt rm_star_silent

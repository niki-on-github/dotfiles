# zsh history

setopt INC_APPEND_HISTORY  # Write to the history file immediately
setopt SHARE_HISTORY  # Share history between all sessions
setopt HIST_IGNORE_ALL_DUPS  # Delete an old recorded event if a new event is a duplicate
setopt HIST_SAVE_NO_DUPS  # Do not write a duplicate command to the history file
setopt HIST_IGNORE_SPACE  # Do not record an command starting with a space (privacy mode)

HISTSIZE=2500
SAVEHIST=2500

HISTFILE=~/.cache/zhistory

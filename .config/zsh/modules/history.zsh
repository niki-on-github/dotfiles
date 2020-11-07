# zsh history

setopt EXTENDED_HISTORY  # save each commands beginning timestamp and the duration to the history file
setopt HIST_IGNORE_ALL_DUPS  # delete an old recorded event if a new event is a duplicate
setopt HIST_IGNORE_SPACE  # do not record an command starting with a space (privacy mode)
setopt HIST_REDUCE_BLANKS # remove superfluous blanks
setopt HIST_SAVE_NO_DUPS  # do not write a duplicate command to the history file
setopt HIST_VERIFY # show the substituted command in the prompt before executing
setopt INC_APPEND_HISTORY  # write to the history file immediately
setopt SHARE_HISTORY  # share history between all sessions

HISTSIZE=2500
SAVEHIST=2500

HISTFILE=~/.cache/zhistory
HISTORY_IGNORE='(rm *|cd|cd ..|l|la|ga|gaa|gp|gph|gpl|z)'

# vim:ft=zsh ts=2 sw=2 sts=2
#
# Fork of agnoster's Theme - https://gist.github.com/3712874
#
# In order for this theme to render correctly, you will need a
# [Powerline-patched font](https://gist.github.com/1595572).
#
# This Theme defines additional prompt segemts.
#


### Segments of the prompt, default order declaration

if [ "$ZSH_KEYMAP" = "vim" ]; then
  typeset -aHg AGNOSTER_PROMPT_SEGMENTS=(
      promt_vim
      prompt_status
      prompt_context
      prompt_conda
      prompt_virtualenv
      prompt_dir
      prompt_git
      prompt_end
  )
else
  typeset -aHg AGNOSTER_PROMPT_SEGMENTS=(
      prompt_exec_time
      prompt_status
      prompt_context
      prompt_conda
      prompt_virtualenv
      prompt_dir
      prompt_git
      prompt_end
  )
fi

### Segment drawing
# A few utility functions to make it easy and re-usable to draw segmented prompts

CURRENT_BG='NONE'
if [[ -z "$PRIMARY_FG" ]]; then
	PRIMARY_FG=black
fi

# Characters
SEGMENT_SEPARATOR="\ue0b0"
PLUSMINUS="\u00b1"
BRANCH="\ue0a0"
DETACHED="\u27a6"
CROSS="\u2718"
LIGHTNING="\u26a1"
GEAR="\uf013"
CHECK="\uf42e"

# Begin a segment
# Takes two arguments, background and foreground. Both can be omitted,
# rendering default background/foreground.
prompt_segment() {
  local bg fg
  [[ -n $1 ]] && bg="%K{$1}" || bg="%k"
  [[ -n $2 ]] && fg="%F{$2}" || fg="%f"
  if [[ $CURRENT_BG != 'NONE' && $1 != $CURRENT_BG ]]; then
    print -n "%{$bg%F{$CURRENT_BG}%}$SEGMENT_SEPARATOR%{$fg%}"
  else
    print -n "%{$bg%}%{$fg%}"
  fi
  CURRENT_BG=$1
  [[ -n $3 ]] && print -n $3
}

# End the prompt, closing any open segments
prompt_end() {
  if [[ -n $CURRENT_BG ]]; then
    print -n "%{%k%F{$CURRENT_BG}%}$SEGMENT_SEPARATOR"
  else
    print -n "%{%k%}"
  fi
  print -n "%{%f%}"
  CURRENT_BG=''
}

### Prompt components
# Each component will draw itself, and hide itself if no information needs to be shown

# Context: user@hostname (who am I and where am I)
prompt_context() {
  local user=`whoami`

  if [[ -n "$SSH_CONNECTION" ]]; then
    prompt_segment $PRIMARY_FG default " %(!.%{%F{yellow}%}.)$user@%m "
  fi
}

# Git: branch/detached head, dirty status
prompt_git() {
  local color ref
  is_dirty() {
    test -n "$(git status --porcelain --ignore-submodules)"
  }
  ref="$vcs_info_msg_0_"
  if [[ -n "$ref" ]]; then
    if is_dirty; then
      color=yellow
      ref="${ref} $PLUSMINUS"
    else
      color=green
      ref="${ref} "
    fi
    if [[ "${ref/.../}" == "$ref" ]]; then
      ref="$BRANCH $ref"
    else
      ref="$DETACHED ${ref/.../}"
    fi
    prompt_segment $color $PRIMARY_FG
    print -n " $ref"
  fi
}

# Dir: current working directory
prompt_dir() {
  prompt_segment blue $PRIMARY_FG ' %3~ '
}

# Status:
# - was there an error
# - am I root
# - are there background jobs?
prompt_status() {
  local symbols
  symbols=()
  [[ $RETVAL -ne 0 ]] && symbols+="%{%F{red}%}$CROSS"
  [[ $UID -eq 0 ]] && symbols+="%{%F{yellow}%}$LIGHTNING"
  [[ $(jobs -l | wc -l) -gt 0 ]] && symbols+="%{%F{cyan}%}$GEAR"
  [[ -z "$symbols" ]] && symbols+="%{%F{green}%}$CHECK"
  [[ -n "$symbols" ]] && prompt_segment '#fcfcfc' default "$symbols "
}

prompt_time() {
    prompt_segment 'blue' 'black' " $(date +"%H:%M:%S")"
}

typeset -g pr_exec_time
typeset -g _pr_exec_time_timer

function _pr_exec_time_preexec() {
  _pr_exec_time_timer=${_pr_exec_time_timer:-$SECONDS}
}

function _pr_exec_time() {
  if [[ -n $_pr_exec_time_timer ]]; then
    local pr_time_spend=$(($SECONDS - $_pr_exec_time_timer))
    pr_exec_time="$pr_time_spend"
    _pr_exec_time_timer=''
  fi
}

autoload -Uz add-zsh-hook
add-zsh-hook preexec _pr_exec_time_preexec
add-zsh-hook precmd _pr_exec_time

prompt_exec_time() {
  if [ -n "$pr_exec_time" ]; then
    local out total_seconds=$pr_exec_time
    local hours=$(( total_seconds / 60 / 60 % 24 ))
    local minutes=$(( total_seconds / 60 % 60 ))
    local seconds=$(( total_seconds % 60 ))
    (( hours > 0 )) && out+="${hours}h "
    (( minutes > 0 )) && out+="${minutes}m "
    out+="${seconds}s"
    prompt_segment '#fcfcfc' 'black' " ${out} "
  else
    prompt_segment '#fcfcfc' 'black' " 0s "
  fi
}

promt_vim() {
    [ -n "${vim_mode}" ] && prompt_segment '#fcfcfc' 'black' " ${vim_mode} "
}

prompt_conda() {
    condaEnvName=""
    if [ -n "$CONDA_DEFAULT_ENV" ]; then
        [ "$CONDA_DEFAULT_ENV" = "base" ] || \
            condaEnvName=" $CONDA_DEFAULT_ENV "
    fi
    prompt_segment '#a0a0a0' 'black' "$condaEnvName"
}

# Display current virtual environment
prompt_virtualenv() {
  if [[ -n $VIRTUAL_ENV ]]; then
    color=cyan
    prompt_segment $color $PRIMARY_FG
    print -Pn " $(basename $VIRTUAL_ENV) "
  fi
}

## Main prompt
prompt_agnoster_main() {
  RETVAL=$?
  CURRENT_BG='NONE'
  for prompt_segment in "${AGNOSTER_PROMPT_SEGMENTS[@]}"; do
    [[ -n $prompt_segment ]] && $prompt_segment
  done
}

prompt_agnoster_precmd() {
  vcs_info
  PROMPT='%{%f%b%k%}$(prompt_agnoster_main) '
}

prompt_agnoster_setup() {
  autoload -Uz add-zsh-hook
  autoload -Uz vcs_info

  prompt_opts=(cr subst percent)

  add-zsh-hook precmd prompt_agnoster_precmd

  zstyle ':vcs_info:*' enable git
  zstyle ':vcs_info:*' check-for-changes false
  zstyle ':vcs_info:git*' formats '%b'
  zstyle ':vcs_info:git*' actionformats '%b (%a)'
}

prompt_agnoster_setup "$@"

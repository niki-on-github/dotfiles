#!/bin/bash

CURRENT_PANE_WIDTH=$(echo "$(tmux lsp -F "#{pane_active} #{window_width}" | grep "^1 " | head -n1 | awk '{print $2}') * 1" | bc)
CURRENT_PANE_HEIGHT=$(echo "$(tmux lsp -F "#{pane_active} #{window_height}" | grep "^1 " | head -n1 | awk '{print $2}') * 2" | bc)
IS_WIDESCREEN=$(echo "2 * $CURRENT_PANE_HEIGHT < $CURRENT_PANE_WIDTH" | bc)

if [ "$IS_WIDESCREEN" = "1" ]; then
    TERMINAL_POSITION='right'
else
    TERMINAL_POSITION='bottom'
fi

current_path() {
    current_path=$(tmux lsp -F "#{pane_active} #{pane_current_path}" | grep "^1 " | head -n1)
    current_path="${current_path:2}"  # remove pane_active information
    echo "$current_path"
}

toggle_terminal() {
    if tmux lsp -F "#{pane_title}" | grep "^lf-terminal$" >/dev/null ; then
        tmux killp -t $(tmux lsp -F "#{pane_id} #{pane_title}" | grep " lf-terminal$" | awk '{print $1}')
        tmux select-layout even-horizontal
    else
        cd "$(current_path)"
        if [ "$TERMINAL_POSITION" = 'bottom' ]; then
            tmux split-window -v -p 30 -f 'printf "\033]2;%s\033\\" "lf-terminal" && exec zsh'
        elif [ "$TERMINAL_POSITION" = 'right' ]; then
            tmux split-window -h -f 'printf "\033]2;%s\033\\" "lf-terminal" && exec zsh'
            tmux select-layout even-horizontal
        elif [ "$TERMINAL_POSITION" = 'left' ]; then
            tmux split-window -h -b -f 'printf "\033]2;%s\033\\" "lf-terminal" && exec zsh'
            tmux select-layout even-horizontal
        fi

        lfTerminalPaneNumber=$(tmux lsp -F "#{pane_id} #{pane_title}" | grep " lf-terminal$" | awk '{print $1}')
        tmux send-keys -t $lfTerminalPaneNumber C-l  # clear terminal
    fi
}

cd_terminal() {
    if tmux lsp -F "#{pane_title}" | grep -q "^lf-terminal$" ; then
        lfTerminalPaneNumber=$(tmux lsp -F "#{pane_id} #{pane_title}" | grep " lf-terminal$" | awk '{print $1}')
        tmux send-keys -t $lfTerminalPaneNumber C-l  # clear terminal
        tmux send-keys -t $lfTerminalPaneNumber C-u  # clear current line
        tmux send-keys -t $lfTerminalPaneNumber " cd \"$(current_path)\"" C-m
    fi
}

split() {
    if [ "$(tmux lsp -F "#{pane_title}" | grep "^lf-pane-" | wc -l)" -ge "2" ] ; then
        active_lf_pane_index=$(tmux lsp -F "#{pane_active} #{pane_index} #{pane_title}" | grep "lf-pane-" | grep "^1 " | awk '{print $2}')
        [ -z "$active_lf_pane_index" ] && return
        eval "tmux killp -t $active_lf_pane_index"
        if [ "$TERMINAL_POSITION" = 'left' ] || [ "$TERMINAL_POSITION" = 'right' ]; then
            tmux select-layout even-horizontal
        fi
    else
        cd "$(current_path)"
        tmux split-window -h "exec lf"
        if [ "$TERMINAL_POSITION" = 'left' ] || [ "$TERMINAL_POSITION" = 'right' ]; then
            tmux select-layout even-horizontal
        fi
    fi
}

swap() {
    if tmux lsp -F "#{pane_title}" | grep "^lf-pane-1$" >/dev/null ; then
        if tmux lsp -F "#{pane_title}" | grep "^lf-pane-2$" >/dev/null ; then
            paneId1=$(tmux lsp -F "#{pane_id} #{pane_title}" | grep "lf-pane-1$" | awk '{print $1}')
            paneId2=$(tmux lsp -F "#{pane_id} #{pane_title}" | grep "lf-pane-2$" | awk '{print $1}')
            tmux swap-pane -s $paneId1 -t $paneId2
        fi
    fi
}

new_tab() {
    tmux set-option -g status on
    tmux new-window lf
}


reset_lf() {
    # The lf sever unfortunately crashes regularly. After a restart the functionality of still open lf clients is severely limited because
    # the id of the lf instance is no longer valid. Workaround is this reset function to continue working without manualy restart the lf client.

    # first save current state
    pane_id=$(tmux lsp -F "#{pane_active} #{pane_title} #{pane_id}" | grep "^1 " | grep "lf-pane" | cut -d ' ' -f 3-)
    num_panes=$(tmux lsp | wc -l)
    num_lf_panes=$(tmux lsp -F "#{pane_title}" | grep "lf-pane" | wc -l)

    other_pane_path=$(tmux lsp -F "#{pane_active} #{pane_title} #{pane_current_path}" | grep "^0 " | grep "lf-pane" | cut -d ' ' -f 3-)
    if [ -n "$other_pane_path" ]; then
        other_pane_id=$(tmux lsp -F "#{pane_active} #{pane_title} #{pane_id}" | grep "^0 " | grep "lf-pane" | cut -d ' ' -f 3-)
        tmux split-window -h "cd $other_pane_path && exec lf"
        tmux kill-pane -t $other_pane_id
    fi

    tmux split-window -h 'exec lf'
    tmux kill-pane -t $pane_id

    if [ "$num_panes" = "3" ]; then
        if [ "$TERMINAL_POSITION" = 'bottom' ]; then
            tmux select-layout tiled
        else
            tmux select-layout even-horizontal
        fi
    elif [ "$num_panes" = "2" ]; then
        if [ "$num_lf_panes" = "2" ]; then
            tmux select-layout even-horizontal
        else
            tmux select-layout even-vertical
        fi
    fi
}

quit() {
    # quit current lf pane
    tmux kill-pane
}

quit_all() {
    # quit all lf panes
    tmux kill-window
}

[ -z "$1" ] && exit

if [[ -v TMUX ]]; then
    case $1 in
        terminal|--terminal) toggle_terminal; exit ;;
        cd-terminal|--cd-terminal) cd_terminal; exit ;;
        split|--split) split; exit;;
        swap|--swap) swap; exit ;;
        new-tab|--new-tab) new_tab; exit ;;
        reset-lf|--reset-lf) reset_lf; exit;;
        quit|--quit) quit; exit ;;
        quit-all|--quit-all) quit_all; exit ;;
    esac
fi

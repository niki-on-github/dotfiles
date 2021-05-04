#!/usr/bin/env bash
# Description: Lists files from remote servers and rsyncs them to the current directory
# Usage: add to ~/.config/lfrc: 'cmd ssh-get &{{ ~/.config/lf/scripts/ssh-get.sh "$id" "$@" }}'
# Alternatively, the shell pipe can be used: 'cmd ssh-get %{{ ... }}'

[ -z "$SUDO_ASKPASS" ] && notify-send "Warning" "Environment variable \$SUDO_ASKPASS is not set!"
command -v inotifywait >/dev/null || ( notify-send "Error" "Please install \"inotify-tools\"" && exit 1 )


declare -A domains=()
declare -A paths=()
declare -a files=()
declare -r tmpDir=$(mktemp -d)
declare -r tmpSSHKeyFile="${tmpDir}/ssh-get.$RANDOM"
declare -r tmpSelectionFile="${tmpDir}/select.$RANDOM"
declare -r fifo="${tmpDir}/ssh-get.fifo"


print() {
    echo \"$1\"
    [ ! -z "$lfid" ] && lf -remote "send $lfid echo \"$1\""
}

usage() {
    print 'usage: ssh-get <user@host1:/path/to/search> <user@host2:/path/to/search> ...'
    exit
}

cleanup() {
    # remove temp authorized key from server
    if [ -e ${tmpSSHKeyFile}.pub ]; then
        local pubKeyString="$(awk '{print $2}' ${tmpSSHKeyFile}.pub)"
        for s in "${!domains[@]}"; do
            ssh -o PasswordAuthentication=no -i ${tmpSSHKeyFile} "${domains[$s]}" \
            'if test -f $HOME/.ssh/authorized_keys; then if grep -v "'${pubKeyString}'" $HOME/.ssh/authorized_keys > $HOME/.ssh/tmp; then cat $HOME/.ssh/tmp > $HOME/.ssh/authorized_keys && rm $HOME/.ssh/tmp; else rm $HOME/.ssh/authorized_keys && rm $HOME/.ssh/tmp; fi; fi' >/dev/null 2>&1
        done
    fi

    [ -e $fifo ] && rm "$fifo"
    [ -d $tmpDir ] && rm -rf $tmpDir
}
# Performe necessary cleanup operations even when something unexpected goes wrong.
trap cleanup SIGHUP SIGINT SIGTERM EXIT

lfid=$1; shift
(( $# < 1 )) && usage

[ -e $fifo ] && rm -f $fifo
mkfifo "$fifo"


for a; do
    host="${a%:*}"
    path="${a##*:}"
    domains+=( ["$a"]="$host" )
    paths+=( ["$a"]="$path" )
    shift
done

#NOTE: We only use 2048 bit to speed up key generation
ssh-keygen -b 2048 -t rsa -f "${tmpSSHKeyFile}" -q -N ""

for s in "${!domains[@]}"; do
    print 'Login to '${domains[@]}''
    #NOTE: To avoid to try to ask for password on the tty we use setsid (This use the SSH_ASKPASS environment variabel)
    if ! setsid ssh-copy-id -i ${tmpSSHKeyFile} ${domains[$s]} >/dev/null 2>&1 ; then
        notify-send "Error" "Remote connection to server \"${domains[$s]}\" failed"
        echo -n "" >> "$fifo" & # simulate empty fifo input
        continue
    fi

    if ! ssh -i ${tmpSSHKeyFile} "${domains[$s]}" "command -v rsync" >/dev/null ; then
        notify-send "Error" "Please install \"rsync\" on remote computer \"${domains[$s]}\""
        echo -n "" >> "$fifo" & # simulate empty fifo input
        continue
    fi

    ssh -i ${tmpSSHKeyFile} "${domains[$s]}" "find ${paths[$s]}" | sed -r "s|^|${domains[$s]}:|" >> "$fifo" &
done

[ -e $tmpSelectionFile ] && rm -rf $tmpSelectionFile
# run fzf in lf shell mode
lf -remote "send $lfid \$((fzf -e --multi --bind ctrl-a:select-all,ctrl-d:deselect-all,ctrl-t:toggle-all < $fifo) > $tmpSelectionFile)"

# use the inotify kernel subsystem to efficiently wait for the close file event of fzf selection
while read i; do if [ "$i" = "$(basename $tmpSelectionFile)" ]; then break; fi; done \
   < <(inotifywait  -e close --format '%f' --quiet $tmpDir --monitor)

mapfile -t files < $tmpSelectionFile
[ -z "${files[@]}" ] && exit

if (( ${#files[@]} )); then
    print 'rsync downloading ...'
    rsync --protect-args -au --chmod=Du=rwx,Dg=rx,Do=rx,Fu=rw,Fg=r,Fo=r --info=progress2 -e "ssh -i ${tmpSSHKeyFile}" "${files[@]}" . \
        | stdbuf -i0 -o0 -e0 tr '\r' '\n' \
        | while read line; do
            lf -remote "send $lfid echo $line"
        done
    [ ! -z "$lfid" ] && lf -remote "send $lfid reload" # show new files
    notify-send "File Manager" "rsync download completed"
    print 'rsync download completed'
    # lf -remote "send $id echo \"rsync check ...\""
    # sleep 1
    # if ! rsync --protect-args -c -e "ssh -i ${tmpSSHKeyFile}" "${files[@]}" . ; then
    #     print 'rsync download failed'
    #     notify-send --urgency critical "File Manager" "rsync download failed"
    # else
    #     print 'rsync download completed'
    #     notify-send "File Manager" "rsync download completed"
    # fi
fi


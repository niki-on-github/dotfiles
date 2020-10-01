#!/bin/bash
# Description: bitwarden dmenu

CA_PATH="/path/to/self-signed-ca.crt"
[ ! -f $CA_PATH ] && echo "self signed ca not found: $CA_PATH" && exit 1

bw logout
export BW_SESSION="$(NODE_EXTRA_CA_CERTS=$CA_PATH bw login --raw)"

#choice="$(bw list items | jq '.[] | .name' | sort | uniq | fzf)" || exit
choice="$(bw list items | jq '.[] | .name' | sort | uniq | dmenu -l 20)" || exit
[ -z "$choice" ] && exit
jsonObject=$(bw list items | jq '.[] | select(.name=='$choice') | {username: .login.username, password: .login.password}')
echo "$jsonObject"

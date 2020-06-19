#!/usr/bin/env sh
#
# Description:
# This script uses keepassxc-cli to load passwords and other properties
# from the keepassxc database, and dmenu to allow the user to select an entry.
#
# If the user selects the Password, it uses keepassxc to load the password,
# copy it in the clipboard and delete it after some time.
#
# Dependencies: keepassxc, notify-send, xclip, dmenu, (Xdialog)


### CONFIG ###

# Key file (if file note exist, we use password promt)
KEY_FILE=$HOME/.local/share/keepassxc/keyfile.key
# Database file
DB_FILE=$HOME/.local/share/keepassxc/passwords.kdbx
# Start from specified entry
SELECTED_ENTRY=""
# Timeout before the password is deleted from clipbord
TIMEOUT=10

[ -z "$DMENU_STYLE" ] && DMENU_STYLE=""


### FUNCTIONS ###

show_entries() {
	[ ! -z "$1" ] && SELECTED_ENTRY=$SELECTED_ENTRY/${1/\//}

	# Get entries
    if [ ! -z "$KEY_FILE" ]; then
        entries="$(keepassxc-cli ls -k "$KEY_FILE" "$DB_FILE" "$SELECTED_ENTRY" --no-password)"
    else
        entries="$(echo $PASSWORD | keepassxc-cli ls "$DB_FILE" "$SELECTED_ENTRY")"
    fi

    [ $? -ne 0 ] && notify-send "KeepassXC" "Decrypt of $DB_FILE failed" && exit 1
    ENTRY="$(echo "$entries" | grep -v "Passwort eingeben, " | eval "dmenu -i -l 10 $DEMENU_STYLE")" || exit
}

get_pass() {
	notify-send "$SELECTED_ENTRY/$1" "Password copied to clipboard for $TIMEOUT seconds" -t $(( TIMEOUT * 1000 ))
    if [ ! -z "$KEY_FILE" ]; then
        keepassxc-cli clip -q -k $KEY_FILE $DB_FILE "$SELECTED_ENTRY/$1" --no-password $TIMEOUT
    else
	    echo "$PASSWORD" | keepassxc-cli clip -q $DB_FILE "$SELECTED_ENTRY/$1" $TIMEOUT
    fi
}

show_entry_contents() {
    if [ ! -z "$KEY_FILE" ]; then
        contents=$(keepassxc-cli show -k "$KEY_FILE" "$DB_FILE" "$SELECTED_ENTRY/$1" --no-password | grep -v 'Password: ' | grep -v 'Title: ') || exit
    else
	    contents=$(echo "$PASSWORD" | keepassxc-cli show "$DB_FILE" "$SELECTED_ENTRY/$1" | grep -v 'Password: ' | grep -v 'Title: ') || exit
    fi

	choice=$(echo -e "Password: ***\n$contents" | grep -v "Passwort eingeben, " | eval "dmenu -i -l 10 $DMENU_STYLE") || exit
	if [ "$choice" = "Password: ***" ]; then
		get_pass "$1"
	else
		label=$(echo $choice | sed 's/:.*$//g')
		value=$(echo $choice | sed 's/^[A-Za-z]*: //g')
		notify-send "$SELECTED_ENTRY/$1" "$label copied to clipboard"
        echo "$label: $value"
		echo -n $value | xclip -selection clipboard
	fi

    exit
}


### MAIN ###

[ ! -f $DB_FILE ] && notify-send "ERROR" "FileNotFound: $DB_FILE" && exit 1

if [ ! -f "$KEY_FILE" ]; then
    KEY_FILE=""
    if [ -z "$SUDO_ASKPASS" ]; then
        PASSWORD=$(Xdialog --stdout --wmclass "pop-up" --title "KeepassXC Database Password" --left --password=1 --inputbox "KeepassXC Database Password" 14 50)

    else
        PASSWORD=$($SUDO_ASKPASS "KeepassXC Password:")
    fi
    [ -z "$PASSWORD" ] && exit 1
fi

show_entries

# If $ENTRY ends in /, show child entries
while [[ $ENTRY = *\/ ]]; do
    show_entries $ENTRY
done

# When a leaf entry is selected, show the entry's contents
if [ ! -z "$ENTRY" ]; then
    show_entry_contents "$ENTRY"
fi

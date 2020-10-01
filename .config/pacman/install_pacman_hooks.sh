#!/bin/bash

[ "$EUID" -ne 0 ] && echo "Please run script with sudo" && exit 1
[ ! -d "$PWD/hooks" ] && echo "FolderNotFound: $PWD/hooks" && exit 1
cp -rvf $PWD/hooks /etc/pacman.d/hooks

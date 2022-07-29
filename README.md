# .dotfiles

A Repository to backup, restore and sync my configurations and scripts (which gives me my super powers). Feel free to look around. Be aware that I have configured my environment to fit my workflow.

## Getting Started

1. Set required packages:

```bash
sudo pacman -Sy fzf git git-lfs zsh
git lfs install >/dev/null 2>&1 || echo "initialize git lfs"
sudo usermod -s /usr/bin/zsh $USER
```

2. Install dotfiles:

```bash
git clone --recursive --recurse-submodules --remote --bare [URL] ~/.dotfiles
git --git-dir=$HOME/.dotfiles --work-tree=$HOME checkout -f master
git --git-dir=$HOME/.dotfiles --work-tree=$HOME config --local status.showUntrackedFiles no
reboot
dotfiles-update
```

## RK61 Shortcuts

- Arrow key function layer on/off: `Fn + Enter`
- Windows Mode: `Fn + a`
- Mac Mode: `Fn + s`

### Navigation without Arrow Keys

#### Emacs

- `CTRL + f`: forward/right one character
- `CTRL + b`: back/left one character
- `ESC + f`: forward/right one word
- `ESC + b`: back/left one word
- `CTRL + a`: jump to the beginning of the line
- `CTRL + e`: jump to the end of the line

#### dmenu

- `CTRL + f`: right
- `CTRL + b`: left
- `CTRL + e`: end
- `CTRL + a`: first element
- `CTRL + n`: down
- `CTRL + p`: up

## References

- https://github.com/LukeSmithxyz/voidrice <br>
- https://github.com/BrodieRobertson/dotfiles <br>
- https://github.com/yramagicman/stow-dotfiles <br>
- https://gitlab.com/dwt1/dotfiles <br>
- https://github.com/felipefacundes/dotfiles <br>
- https://github.com/altercation/dotfiles-tilingwm <br>
- https://github.com/NapoleonWils0n <br>
- https://github.com/jessfraz/dotfiles <br>
- https://github.com/altindas/stillarch <br>
- https://pbrisbin.com/tags/xmonad <br>
- https://github.com/nathanshelly/.files <br>
- https://github.com/eoli3n/dotfiles <br>
- https://github.com/SwiftyChicken/dotfiles <br>
- https://github.com/nilcons/firefox-hacks <br>
- https://github.com/MrOtherGuy/firefox-csshacks <br>
- https://github.com/ivanbrennan/nixbox <br>
- http://kb.mozillazine.org/About:config_entries <br>

# .dotfiles (Arch Linux)

A Repository to make a subset of my dotfiles public. Feel free to look around. Be aware that I have configured my environment to fit my workflow.

## Getting Started

1. Set zsh:

```bash
sudo pacman -Sy zsh
sudo usermod -s /usr/bin/zsh $USER
```

2. Install dotfiles:

```bash
git clone --recursive --remote --bare [URL] ~/.dotfiles
git --git-dir=$HOME/.dotfiles --work-tree=$HOME checkout -f master
git --git-dir=$HOME/.dotfiles --work-tree=$HOME config --local status.showUntrackedFiles no
reboot
```

## References

- https://github.com/LukeSmithxyz/voidrice <br>
- https://github.com/BrodieRobertson/dotfiles <br>
- https://gitlab.com/dwt1/dotfiles <br>
- https://github.com/altercation/dotfiles-tilingwm <br>

#!/bin/bash
# Description: xmonad stack-static install and update script
# Documentation: https://brianbuccola.com/how-to-install-xmonad-and-xmobar-via-stack/

# set -e

if [ "$EUID" -eq 0 ]
  then echo "please run without root privileges"
  exit 1
fi

if [ ! -f ./xmonad.hs ]; then
    echo "please run this script from your xmonad config directory"
    exit 1
fi

if ! command -v stack; then
    paru --needed -Sy stack-static
fi

sudo pacman --noconfirm -R xmonad xmobar xmonad-utils xmonad-contrib
sudo pacman --needed -Sy trayer

stack setup

if [ ! -f stack.yaml ]; then
    echo "Create stack.yaml"
    echo "INFO: If build failed you have to manually edit the generated stack.yaml"
    stack init
fi

stack install

sudo mkdir -p /usr/share/xsessions

if [ ! -f build ]; then
    cat >/usr/share/xsessions/xmonad.desktop <<EOL
# ~/.config/xmonad/build
#!/bin/sh
exec stack ghc -- \
  --make xmonad.hs \
  -i \
  -ilib \
  -fforce-recomp \
  -main-is main \
  -v0 \
  -o "\$1"
EOL
fi

chmod a+x build

if [ ! -f /usr/share/xsessions/xmonad.desktop ]; then
    echo "Add login manager entry"
    sudo bash -c "cat >/usr/share/xsessions/xmonad.desktop" <<EOL
[Desktop Entry]
Encoding=UTF-8
Type=Application
Name=Xmonad
Comment=Lightweight X11 tiled window manager written in Haskell
Exec=xmonad
Icon=xmonad
Terminal=false
StartupNotify=false
Categories=Application;
EOL
fi

if ! grep -q ".local/bin" <<< "$PATH" ; then
    echo "Add ~/.local/bin to \$PATH"
    echo "export PATH=\"\${HOME}/.local/bin:\${PATH}\"" >> ~/.profile
fi

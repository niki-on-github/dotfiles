# .profile - Runs on login, Environmental variables are set here

# set zsh config directory
export ZDOTDIR="${XDG_CONFIG_HOME:-$HOME/.config}/zsh"

# base environment variables
export BROWSER="firefox"
export COLORTERM="truecolor"
export EDITOR="nvim"
export EDITOR_GUI="kwrite"
export OPENER="xdg-open"
export PAGER="less"
export READER="zathura"
export READER_GUI="firefox"
export TERMINAL="alacritty"
export VISUAL="nvim"
export XCURSOR_THEME="breeze_cursors"
export WALLPAPER_PATH="$HOME/Nextcloud/Pictures/Backgrounds/21_9"
export LESSHISTFILE=-

# clipboard
export CM_SELECTIONS="clipboard"
export CM_MAX_CLIPS=50

# Qt5 apps theme
[ "$XDG_CURRENT_DESKTOP" = "KDE" ] || export QT_QPA_PLATFORMTHEME="qt5ct"

# X11
# if [ "$XDG_SESSION_TYPE" = "x11" ]; then
#     # fix: same alacritty font size in X11 and wayland
#     export WINIT_X11_SCALE_FACTOR=1
# fi

# Wayland
if [ "$XDG_SESSION_TYPE" = "wayland" ]; then
    export MOT_ENABLE_WAYLAND=1
    export QT_QPA_PLATFORM="wayland"
fi

# Remove username in agnoster prompt
export DEFAULT_USER=$(whoami)

# Set dmenu style (this override the xresource defaults if used in your scripts)
# [old] export DMENU_STYLE="-fn 'Hack-12' -h 25 -nf '#FFFFFF' -nb '#000000' -sf '#FFFFFF' -sb '#1D99F3'"
export DMENU_STYLE=""  # currently we set dmenu theme via .Xresources

# Use dmenu for sudo/ssh password prompt (alternatively use 'ksshaskpass' on a KDE DE)
export SUDO_ASKPASS="$HOME/.config/dmenu/askpass.sh"
export SSH_ASKPASS="$HOME/.config/dmenu/askpass.sh"

# Set where pass store your passwords
export PASSWORD_STORE_DIR="$HOME/.local/share/password-store"

# my fzf default colors (see https://github.com/junegunn/fzf/wiki/Color-schemes)
export FZF_DEFAULT_OPTS='
  --color=bg+:-1,bg:-1,spinner:#1d99f3,hl:#1d99f3,gutter:-1
  --color=fg:-1,header:#1d99f3,info:#707070,pointer:#d7005f
  --color=marker:#1d99f3,fg+:#ffffff,prompt:#d7005f,hl+:4'

# Xmonad
mkdir -p $HOME/.cache/xmonad
mkdir -p $HOME/.local/share/xmonad
export XMONAD_CONFIG_DIR="$HOME/.config/xmonad"
export XMONAD_CACHE_DIR="$HOME/.cache/xmonad"
export XMONAD_DATA_DIR="$HOME/.local/share/xmonad"

# lf icons (Require Nerd Font e.g. in ~/.fonts, use https://www.nerdfonts.com/cheat-sheet to find icons)
export LF_ICONS="di=:fi=:ln=:or=:ex=:*.c=:*.cc=:*.clj=:*.coffee=:*.cpp=:*.css=:*.d=:*.dart=:*.erl=:*.exs=:*.fs=:*.go=:*.h=:*.hh=:*.hpp=:*.hs=:*.html=:*.java=:*.jl=:*.js=:*.json=:*.lua=:*.md=:*.php=:*.pl=:*.pro=:*.py=:*.rb=:*.rs=:*.scala=:*.ts=:*.vim=:*.cmd=:*.ps1=:*.sh=:*.bash=:*.zsh=:*.fish=:*.tar=:*.tgz=:*.arc=:*.arj=:*.taz=:*.lha=:*.lz4=:*.lzh=:*.lzma=:*.tlz=:*.txz=:*.tzo=:*.t7z=:*.zip=:*.z=:*.dz=:*.gz=:*.lrz=:*.lz=:*.lzo=:*.xz=:*.zst=:*.tzst=:*.bz2=:*.bz=:*.tbz=:*.tbz2=:*.tz=:*.deb=:*.rpm=:*.jar=:*.war=:*.ear=:*.sar=:*.rar=:*.alz=:*.ace=:*.zoo=:*.cpio=:*.7z=:*.rz=:*.cab=:*.wim=:*.swm=:*.dwm=:*.esd=:*.jpg=:*.jpeg=:*.mjpg=:*.mjpeg=:*.gif=:*.bmp=:*.pbm=:*.pgm=:*.ppm=:*.tga=:*.xbm=:*.xpm=:*.tif=:*.tiff=:*.png=:*.PNG=:*.svg=:*.svgz=:*.mov=:*.mpeg=:*.mkv=:*.m4v=:*.webm=:*.mp4=:*.wmv=:*.avi=:*.flv=:*.flac=:*.mp3=:.m4a=:*.wav=:*.pdf=:*.iso=:*.img="

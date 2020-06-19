""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" NEOVIM CONFIG
"
" The setup is fully automated. When neovim is called first time, the setup process get invoked. 
" Some error messages are displayed which must be confirmed to complete the setup. Wenn the setup 
" is completed you have to manualy restart neovim to have full access to all defined functions 
" and plugins.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Set <leader> key "
let mapleader =","

" Set <localleader> key "
let maplocalleader="-"


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" VIM Plugin Managing (Install new Plugins with `:PlugInstall`)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Install vim plugin manager "
if ! filereadable(expand('~/.config/nvim/autoload/plug.vim'))
    echo "Downloading vim-plug to manage plugins ..."
    silent !mkdir -p ~/.config/nvim/autoload/
    silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ~/.config/nvim/autoload/plug.vim
    autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.config/nvim/plugged')
" Light status and tabline "
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" Provide mappings to easily delete, change and add parentheses, brackets, quotes, XML tags, and more "
Plug 'tpope/vim-surround'
" Add icons to vim statusline and more "
Plug 'ryanoasis/vim-devicons'
" File system explorer "
Plug 'scrooloose/nerdtree'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'Xuyuanp/nerdtree-git-plugin'
" fzf for vim "
Plug 'junegunn/fzf.vim'
" Git for vim"
Plug 'airblade/vim-gitgutter'
Plug 'samoshkin/vim-mergetool'
Plug 'tpope/vim-fugitive'
" Comment/uncomment helper (gcc to comment out (toggle) a line, gc in visual mode to comment out the selection) "
Plug 'tpope/vim-commentary'
" provides support for LaTeX documents (Required for coc latex extension) "
Plug 'lervag/vimtex'
" color preview "
Plug 'ap/vim-css-color'
" Code Completion / Intellisense Engine "
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" Vim Theme "
Plug 'dikiaap/minimalist'
call plug#end()


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" soruce plugin configs
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

source $HOME/.config/nvim/plug-config/minimalist.vim
source $HOME/.config/nvim/plug-config/coc.vim
source $HOME/.config/nvim/plug-config/nerdtree.vim
source $HOME/.config/nvim/plug-config/gitgutter.vim
source $HOME/.config/nvim/plug-config/mergetool.vim
source $HOME/.config/nvim/plug-config/fzf.vim


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Basic config
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

 " Enable Mouse Support ('a' for 'all') "
set mouse=a
" Use system clipboard (e.g. 'Ctrl+C' in other program and put in vim with p) "
set clipboard=unnamedplus
" Set output encoding that is shown in the terminal "
set encoding=utf-8
" Use hybride line numbers (relative + absolute line numbers) "
set number relativenumber
" Disable mode output in powerline (already exists in the airline statusbar) "
set noshowmode
" Enable autocompletion for command line "
set wildmode=longest,list,full
" Disables automatic commenting on newline "
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
" Splits open at the bottom and right "
set splitbelow splitright
" Shortcutting split navigation (saving a keypress) "
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
" highlight all search matches "
set hlsearch
" Press ESC to turn off seach highlighting and clear any message already displayed.
nnoremap <silent> <ESC> :nohlsearch<Bar>:echo<CR>
" Press Space to turn off spell highlighting and clear any messages already displayed.
nnoremap <silent> <Space> :set<space>nospell<Bar>:echo<CR>
" Press F4 to toggle highlighting on/off, and show current value.
noremap <F4> :set hlsearch! hlsearch?<CR>
" I frequently type :W instead of :w and :Q insetd of :q, because the colon needs a shift, w and q doesn't"
command WQ wq
command Wq wq
command W w
command Q q
" Use smart tabs "
set expandtab
set smarttab
" Use 4 spaces = 1 tab "
set shiftwidth=4
set tabstop=4
set softtabstop=4
"fix for yankring "
let g:yankring_clipboard_monitor=0


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Helper
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Spell-check "
map <leader>s :setlocal spell! spelllang=de_de<CR>
" Sugest a list of spell alternatives "
nnoremap <silent> z z=
" Open bibliography file in split "
if filereadable(expand("./Literatur.bib"))
    map <leader>b :vsp<space>./Literatur.bib<CR>
elseif filereadable(expand("../Literatur.bib"))
    map <leader>b :vsp<space>../Literatur.bib<CR>
endif
" Open abbreviations file in split "
if filereadable(expand("./Abkuerzungen.tex"))
    map <leader>a :vsp<space>./Abkuerzungen.tex<CR>
elseif filereadable(expand("../Abkuerzungen.tex"))
    map <leader>a :vsp<space>../Abkuerzungen.tex<CR>
endif
" Compile document (groff, LaTeX, markdown, etc.)
map <leader>c :w! \| !$HOME/.local/bin/compile <c-r>%<CR>
" Open corresponding .pdf/.html
map <leader>p :!$HOME/.config/nvim/scripts/previewer.sh <c-r>%<CR><CR>
" Runs a script that cleans out tex build files whenever I close out of a .tex file.
autocmd VimLeave *.tex !$HOME/.config/nvim/scripts/tex_clean_up.sh %
" Run xrdb whenever Xdefaults or Xresources are updated "
autocmd BufWritePost *Xresources,*Xdefaults !xrdb %


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Filetype
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Enable filetype by matching a given pattern (Required for some vim plugins)"
filetype plugin indent on
" Ensure files are read as what I want "
autocmd BufRead,BufNewFile *.ms,*.me,*.mom,*.man set filetype=groff
autocmd BufRead,BufNewFile *.tex set filetype=tex
autocmd BufRead,BufNewFile /tmp/calcurse*,~/calcurse/notes/* set filetype=markdown
autocmd BufRead,BufNewFile *.md,*.MD set filetype=markdown


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Bindings and Aliase
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" paste in a new line "
nmap P :pu<CR>
" replace all (:%s/Search/Replace/Flags)"
nnoremap R :%s//g<Left><Left>


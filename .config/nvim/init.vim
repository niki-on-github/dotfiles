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
" VIM Plugin Managing (Install new Plugins with `:PlugInstall`, Update with `:PlugUpdate` + `:CocUpdate`)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Install vim plugin manager "
if ! filereadable(expand('~/.config/nvim/autoload/plug.vim'))
    echo "Downloading vim-plug to manage plugins ..."
    silent !mkdir -p ~/.config/nvim/autoload/
    silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ~/.config/nvim/autoload/plug.vim
    autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.config/nvim/plugged')
" status and tabline "
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" Provide mappings to easily delete, change and add parentheses, brackets, quotes, XML tags, and more "
Plug 'tpope/vim-surround'
" Add icons to vim statusline and more "
Plug 'ryanoasis/vim-devicons'
" fzf for vim "
Plug 'junegunn/fzf.vim'
" highlight trailing whitespace characters "
Plug 'ntpeters/vim-better-whitespace'
" run git commands with :G <command> and more (better integration in vim than use :!git <command>) "
Plug 'airblade/vim-gitgutter'
" better git mergetool "
Plug 'samoshkin/vim-mergetool'
" show git changes in vim "
Plug 'tpope/vim-fugitive'
" comment/uncomment helper (gcc to comment out (toggle) a line, gc in visual mode to comment out the selection) "
Plug 'tpope/vim-commentary'
" provides support for LaTeX documents "
Plug 'lervag/vimtex'
" Go language support for Vim "
" Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
" vertical lines at each indentation level for code indented with spaces "
Plug 'Yggdroot/indentLine'
" terminal in floating popup window inside vim "
Plug 'voldikss/vim-floaterm'
" Highlights overused words "
Plug 'dbmrq/vim-ditto'
" color preview "
Plug 'ap/vim-css-color'
" synonym checkup "
Plug 'ron89/thesaurus_query.vim'
" much simpler vim motions "
Plug 'easymotion/vim-easymotion'
" Code Completion / Intellisense Engine "
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" My preferred vim theme "
Plug 'dikiaap/minimalist'
" Displays available keybindings in popup "
Plug 'liuchengxu/vim-which-key'
" a other vim theme "
" Plug 'tomasiser/vim-code-dark'
" coc pairs only at line ends, require yarn (sudo pacman -Sy yarn) for installation"
" Plug 'niki-on-github/coc-pairs', {'do': 'yarn install --frozen-lockfile && yarn build'}
call plug#end()


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" soruce plugin configs
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

source $HOME/.config/nvim/plug-config/minimalist.vim
source $HOME/.config/nvim/plug-config/coc.vim
source $HOME/.config/nvim/plug-config/gitgutter.vim
source $HOME/.config/nvim/plug-config/mergetool.vim
source $HOME/.config/nvim/plug-config/fzf.vim
source $HOME/.config/nvim/plug-config/indentline.vim
source $HOME/.config/nvim/plug-config/floaterm.vim
source $HOME/.config/nvim/plug-config/easymotion.vim
source $HOME/.config/nvim/plug-config/vimtex.vim
source $HOME/.config/nvim/plug-config/thesaurusquery.vim
source $HOME/.config/nvim/plug-config/whichkey.vim


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
" Word wrap
set wrap
set linebreak " only wrap at a character in the breakat option (default: ^I!@*-+;:,./?V) "
" Shortcutting split navigation (saving a keypress) "
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
" highlight all search matches "
set hlsearch
" Press ESC to turn off seach highlighting and clear any message already displayed.
nnoremap <silent> <ESC> :nohlsearch<Bar>:echo<CR>
" Press F4 to toggle highlighting on/off, and show current value.
noremap <F4> :set hlsearch! hlsearch?<CR>
" I frequently type :W instead of :w and :Q insetd of :q, because the colon needs a shift, w and q doesn't"
" my vim auto sugestion for commands "
command WQ wq
command Wq wq
command W w
command Q q
map Vs vs
map Sp sp
" Do not show stupid q: window
map q: :q
" Use smart tabs "
set expandtab
set smarttab
" Use 4 spaces = 1 tab "
set shiftwidth=4
set tabstop=4
set softtabstop=4
"fix for yankring "
let g:yankring_clipboard_monitor=0
" show search results in center "
nnoremap n nzzzv
nnoremap N Nzzzv
" jump to line and show in center"
nnoremap G Gzzzv
" make backspace key more powerful "
set backspace=indent,eol,start
" smart case insensitive search (case sensitive when search pattern contains upper case characters)"
set ignorecase
set smartcase
" keep indenting visual block
vmap < <gv
vmap > >gv
map sh SH


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Helper
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" toggle Spell-check (use zg to mark word as good, use zw to mark word as wrong, use z= suggest a list of spell)"
map <leader>s :setlocal spell! spelllang=de_de,en_us<CR>
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
" Filetype-specific indentation
autocmd Filetype yaml setlocal tabstop=2 shiftwidth=2 expandtab
" enable spell check for markdown and latex "
autocmd Filetype markdown,tex setlocal spell spelllang=de_de,en_us


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Filetype
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Enable filetype by matching a given pattern (Required for some vim plugins)"
filetype plugin indent on
" Ensure files are read as what I want "
autocmd BufRead,BufNewFile *.ms,*.me,*.mom,*.man set filetype=groff
autocmd BufRead,BufNewFile *.tex set filetype=tex
autocmd BufRead,BufNewFile *.md,*.MD set filetype=markdown


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Bindings and Aliase
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Control-Backspace to remove last word
inoremap <C-H> <C-W>
" paste in a new line "
nmap P :pu<CR>
" replace all (:%s/Search/Replace/Flags)"
nnoremap R :%s//g<Left><Left>
" change all existing characters to match my vim tab settings "
noremap <Leader>t :retab<CR>
" select all "
nnoremap <C-A> ggVG
" remove trailing space and leave cursor at the last change location and clear output "
nnoremap <Leader>w :let _save_pos=getpos(".") <Bar>
    \ :let _s=@/ <Bar>
    \ :%s/\s\+$//e <Bar>
    \ :let @/=_s <Bar>
    \ :nohl <Bar>
    \ :unlet _s<Bar>
    \ :call setpos('.', _save_pos)<Bar>
    \ :unlet _save_pos<CR>
    \ :echo<CR>

" move current line to the end of the current paragraph and leave cursor on current location "
nnoremap <Leader>e :let _save_pos=getpos(".") <Bar>
    \ :let _s=@/ <Bar>
    \ :m '}-1 <Bar>
    \ :let @/=_s <Bar>
    \ :nohl <Bar>
    \ :unlet _s<Bar>
    \ :call setpos('.', _save_pos)<Bar>
    \ :unlet _save_pos<CR>
    \ :echo<CR>

" Show if there is a tab or not
set list listchars=nbsp:¬,tab:»·,trail:·,extends:>

" Change Space indent of file to my default e.g. Code from Internet have 2-space indenting and I want 4-space indenting.
command Fixtab :exe "normal \g\g\=\G"

" Highlights beginnings of sentences "
command HSentences /\. [^ ]*

if ! filereadable(expand('~/.config/coc/extensions/coc-tabnine-data/binaries'))
    silent !chmod +x ~/.config/coc/extensions/coc-tabnine-data/binaries/*/x86_64-unknown-linux-musl/*
    " if not work use `:CocCommand tabnine.updateTabNine` + y  to force update TabNine
endif

map <F5> :redraw!<CR>

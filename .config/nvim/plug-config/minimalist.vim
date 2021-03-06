set t_Co=256
syntax on
colorscheme minimalist
let g:airline_theme='minimalist'
let g:airline_powerline_fonts = 1
let g:Powerline_symbols='unicode'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#coc#enabled = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Override Theme Defaults

hi CursorLineNr     ctermfg=7     ctermbg=NONE   cterm=NONE
hi WarningMsg       ctermfg=255   ctermbg=1      cterm=NONE
hi ErrorMsg         ctermfg=255   ctermbg=1      cterm=NONE
hi Search           ctermfg=None  ctermbg=4      cterm=NONE
hi SignColumn       ctermfg=NONE  ctermbg=None   cterm=NONE

" auto completion window colors "
hi Pmenu            ctermfg=NONE    ctermbg=233  cterm=NONE
hi PmenuSel         ctermfg=NONE    ctermbg=4    cterm=NONE
hi PmenuSbar        ctermfg=NONE    ctermbg=59   cterm=NONE
hi PmenuThumb       ctermfg=NONE    ctermbg=7    cterm=NONE

" NERDTree color "
hi NERDTreeCWD      ctermfg=4     ctermbg=NONE   cterm=NONE
hi NERDTreeOpenable ctermfg=4     ctermbg=NONE   cterm=NONE
hi NERDTreeClosable ctermfg=4     ctermbg=NONE   cterm=NONE
hi NERDTreeUp       ctermfg=4     ctermbg=NONE   cterm=NONE
hi Number           ctermfg=4     ctermbg=NONE   cterm=NONE

" add floating window color "
hi NormalFloat      ctermfg=NONE  ctermbg=233    cterm=NONE

" update time git marker "
set updatetime=500 "ms"

" color "
hi GitGutterAdd      ctermfg=2 ctermbg=NONE guifg=#00CC00 guibg=NONE
hi GitGutterChange   ctermfg=3 ctermbg=NONE guifg=#AAAA00 guibg=NONE
hi GitGutterDelete   ctermfg=1 ctermbg=NONE guifg=#FF0000 guibg=NONE

" jump between hunks "
nmap ) <Plug>(GitGutterNextHunk)
nmap ( <Plug>(GitGutterPrevHunk)

" set vim-gitgutter defaults "
let g:gitgutter_enabled = 1
let g:gitgutter_signs = 1
let g:gitgutter_highlight_lines = 0
let g:gitgutter_highlight_linenrs = 0
let g:gitgutter_async = 1

" dotfiles bare git repository "
if filereadable(expand("~/.dotfiles/config"))
    let shellcmd = 'git --git-dir=$HOME/.dotfiles --work-tree=$HOME ls-tree -r HEAD --name-only --full-name | xargs -I{} echo "$HOME/{}" | grep '
    " NOTE: the dot in this if query is for concatenating strings "
    :call system(shellcmd . expand('%:p'))
    if !v:shell_error
        " file is a dotfile in my git bare repository "
        autocmd VimEnter * :let g:gitgutter_git_args = "--git-dir=$HOME/.dotfiles --work-tree=$HOME"
    endif
endif

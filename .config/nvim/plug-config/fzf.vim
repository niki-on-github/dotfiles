noremap <silent> <c-p> :Files<CR>
noremap <silent> <c-g> :GFiles<CR>
noremap <silent> <c-f> :BLines<CR>

" vim todo list "
function! s:todoSinkHandler(l)
    let keys = split(a:l, ':')
    execute "edit +". keys[1]. " ". keys[0]
    :echo "open ". keys[0]
endfunction
nnoremap <leader>tl :call fzf#run({'source': 'grep -r -n --exclude-dir="\.git" -i "TODO" .', 'sink': function('<sid>todoSinkHandler')})<CR>

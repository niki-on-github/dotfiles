let g:tq_language=['de', 'en']

" check the synonyms of the word under cursor and replace it with the candidate chosen by user "
" default:  nnoremap <Leader>cs :ThesaurusQueryReplaceCurrentWord<CR>

" query synonym and replace a multi-word phrase covered in visual mode "
" default: vnoremap <Leader>cs y:ThesaurusQueryReplace <C-r>"<CR>

" manually checkup: :Thesaurus [YOUR_PHRASE]

let g:indentLine_conceallevel = 2
" n for Normal mode
" v for Visual mode
" i for Insert mode
" c for Command line editing, for 'incsearch'
let g:indentLine_concealcursor = 'nc'
" disable indentLine plugin in LaTeX and markdown, required because plugin break syntax!"
let g:indentLine_fileTypeExclude = ['tex', 'markdown']
au Filetype tex setlocal conceallevel=0

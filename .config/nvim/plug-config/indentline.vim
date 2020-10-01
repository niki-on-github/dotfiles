" disable indentLine plugin in LaTeX and markdown - required because plugin break syntax!"
let g:indentLine_fileTypeExclude = ['tex', 'markdown']
au Filetype tex setlocal conceallevel=0

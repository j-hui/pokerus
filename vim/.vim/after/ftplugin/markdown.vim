autocmd Filetype markdown syntax sync fromstart
setlocal tabstop=2
setlocal expandtab
setlocal foldlevel=2
setlocal shiftwidth=2
setlocal softtabstop=2
setlocal spell
setlocal formatoptions-=tc

set suffixesadd=.md

" Use soft wrap
setlocal textwidth=0
setlocal wrap
setlocal colorcolumn=0

setlocal breakat-=*         " avoid breaking footnote*
setlocal breakat-=@         " avoid breaking at email addresses

autocmd Filetype markdown syntax sync fromstart
setlocal tabstop=2
setlocal expandtab
setlocal foldmethod=syntax
setlocal shiftwidth=2
setlocal softtabstop=2
setlocal spell
setlocal formatoptions-=tc

set suffixesadd=.md

" Use soft wrap
" setlocal textwidth=0
setlocal wrap
setlocal colorcolumn=0

setlocal breakat-=*         " avoid breaking footnote*
setlocal breakat-=@         " avoid breaking at email addresses

" markdownWikiLink is a new region
syn region markdownWikiLink matchgroup=markdownLinkDelimiter start="\[\[" end="\]\]" contains=markdownUrl keepend oneline concealends
" markdownLinkText is copied from runtime files with 'concealends' appended
syn region markdownLinkText matchgroup=markdownLinkTextDelimiter start="!\=\[\%(\%(\_[^][]\|\[\_[^][]*\]\)*]\%( \=[[(]\)\)\@=" end="\]\%( \=[[(]\)\@=" nextgroup=markdownLink,markdownId skipwhite contains=@markdownInline,markdownLineStart concealends
" markdownLink is copied from runtime files with 'conceal' appended
syn region markdownLink matchgroup=markdownLinkDelimiter start="(" end=")" contains=markdownUrl keepend contained conceal

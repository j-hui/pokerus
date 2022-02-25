function pokerus#plugins#fugitive#setup()
  augroup fugitive_maps
    autocmd!
    autocmd Filetype fugitive,git,floggraph nmap <buffer> q gq
    autocmd User FugitiveStageBlob          nmap <buffer> q :q<CR>
  augroup END
endfunction

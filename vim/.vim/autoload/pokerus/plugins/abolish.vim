function! pokerus#plugins#abolish#setup() abort
  let g:abolish_no_mappings = 1
  if !has('nvim')
    nmap ga <Plug>(abolish-coerce)
    vmap ga <Plug>(abolish-coerce)
    nmap gaa <Plug>(abolish-coerce-word)
  endif
endfunction

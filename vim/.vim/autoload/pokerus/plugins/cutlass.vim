function pokerus#plugins#cutlass#setup()
  " x and D only delete, no yank/cut but retain cut behavior for d
  nnoremap d  d
  xnoremap d  d
  vnoremap d  d
  nnoremap dd dd
endfunction

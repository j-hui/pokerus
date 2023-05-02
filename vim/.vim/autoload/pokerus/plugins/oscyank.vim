function! pokerus#plugins#oscyank#setup() abort
  augroup osc-yank
    autocmd!
    autocmd TextYankPost * if v:event.operator is 'y' && v:event.regname is '' | execute 'OSCYankRegister "' | endif
  augroup END
endfunction

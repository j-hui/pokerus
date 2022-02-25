function pokerus#plugins#oscyank#setup()
  augroup osc-yank
    autocmd!
    autocmd TextYankPost * if v:event.operator is 'y' && v:event.regname is '' | execute 'OSCYankReg "' | endif
  augroup END
endfunction

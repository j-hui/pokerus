function pokerus#plugins#subversive#setup()
  let g:subversivePromptWithActualCommand = 1
  nmap gs <plug>(SubversiveSubstitute)
  nmap gss <plug>(SubversiveSubstituteLine)
  " Paste in visual mode
  xmap p <plug>(SubversiveSubstitute)
  xmap P <plug>(SubversiveSubstitute)
endfunction

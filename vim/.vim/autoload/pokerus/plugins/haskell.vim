function pokerus#plugins#haskell#setup()
  let g:haskell_enable_quantification = 1   " highlight `forall`
  let g:haskell_enable_recursivedo = 1      " highlight `mdo` and `rec`
  let g:haskell_enable_arrowsyntax = 1      " highlight `proc`
  let g:haskell_enable_pattern_synonyms = 1 " highlight `pattern`
  let g:haskell_enable_typeroles = 1        " highlight type roles
  let g:haskell_enable_static_pointers = 1  " highlight `static`
  let g:haskell_backpack = 1                " highlight backpack keywords
endfunction

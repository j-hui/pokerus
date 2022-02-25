function pokerus#plugins#sandwich#setup()
  runtime macros/sandwich/keymap/surround.vim

  if !exists('g:sandwich#recipes')
    let g:sandwich#recipes = []
  endif

  " vim-surround functionality for adding space padding:
  let g:sandwich#recipes += [
      \   {'buns': ['{ ', ' }'], 'nesting': 1, 'match_syntax': 1, 'kind': ['add', 'replace'], 'action': ['add'], 'input': ['{']},
      \   {'buns': ['[ ', ' ]'], 'nesting': 1, 'match_syntax': 1, 'kind': ['add', 'replace'], 'action': ['add'], 'input': ['[']},
      \   {'buns': ['( ', ' )'], 'nesting': 1, 'match_syntax': 1, 'kind': ['add', 'replace'], 'action': ['add'], 'input': ['(']},
      \   {'buns': ['{\s*', '\s*}'],   'nesting': 1, 'regex': 1, 'match_syntax': 1, 'kind': ['delete', 'replace', 'textobj'], 'action': ['delete'], 'input': ['{']},
      \   {'buns': ['\[\s*', '\s*\]'], 'nesting': 1, 'regex': 1, 'match_syntax': 1, 'kind': ['delete', 'replace', 'textobj'], 'action': ['delete'], 'input': ['[']},
      \   {'buns': ['(\s*', '\s*)'],   'nesting': 1, 'regex': 1, 'match_syntax': 1, 'kind': ['delete', 'replace', 'textobj'], 'action': ['delete'], 'input': ['(']},
      \ ]

  " Text object targets for interacting with inner surroundings:
  xmap ib   <Plug>(textobj-sandwich-query-i)
  xmap ab   <Plug>(textobj-sandwich-query-a)
  omap ib   <Plug>(textobj-sandwich-query-i)
  omap ab   <Plug>(textobj-sandwich-query-a)
  xmap ibb  <Plug>(textobj-sandwich-auto-i)
  xmap abb  <Plug>(textobj-sandwich-auto-a)
  omap ibb  <Plug>(textobj-sandwich-auto-i)
  omap abb  <Plug>(textobj-sandwich-auto-a)
  xmap ic   <Plug>(textobj-sandwich-literal-query-i)
  xmap ac   <Plug>(textobj-sandwich-literal-query-a)
  omap ic   <Plug>(textobj-sandwich-literal-query-i)
  omap ac   <Plug>(textobj-sandwich-literal-query-a)


  augroup SandwichFiletypes
    autocmd!
    autocmd FileType python call sandwich#util#addlocal([
      \   {'buns': ['"""', '"""'], 'nesting': 0, 'input': ['3"']},
      \ ])
  augroup END
endfunction

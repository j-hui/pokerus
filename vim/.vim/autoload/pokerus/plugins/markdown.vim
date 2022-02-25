function pokerus#plugins#markdown#setup()
  let g:markdown_fenced_languages = [
      \ 'html',
      \ 'python',
      \ 'bash=sh',
      \ 'c',
      \ 'cpp',
      \ 'ocaml',
      \ 'haskell',
      \ 'rust',
      \ 'lua',
      \ 'vim',
      \ ]
  let g:vim_markdown_fenced_languages = g:markdown_fenced_languages
  let g:vim_markdown_auto_insert_bullets = 0
  let g:vim_markdown_folding_style_pythonic = 1
  " let g:vim_markdown_math = 1
  augroup markdown_mappings
    autocmd!
    autocmd Filetype markdown nmap g] <Plug>Markdown_MoveToCurHeader
    autocmd Filetype markdown nmap g[ <Plug>Markdown_MoveToParentHeader
  augroup END
endfunction

function plugins#editing#setup()
  Plug 'tpope/vim-commentary'
  " use gcc to comment things out

  Plug 'tpope/vim-unimpaired'
  " ]* and [* mappings

  Plug 'tpope/vim-endwise'
  " Write endings

  Plug 'tpope/vim-speeddating'
  " increment/decrement dates

    let g:speeddating_no_mappings = 1
    " force a non-recursive map to the fallback functions
    nnoremap <Plug>SpeedDatingFallbackUp    <C-A>
    nnoremap <Plug>SpeedDatingFallbackDown  <C-X>
    xnoremap <Plug>SpeedDatingFallbackUp    <C-A>
    xnoremap <Plug>SpeedDatingFallbackDown  <C-X>

    nmap + <Plug>SpeedDatingUp
    nmap - <Plug>SpeedDatingDown
    xmap + <Plug>SpeedDatingUp
    xmap - <Plug>SpeedDatingDown

  Plug 'svermeulen/vim-cutlass'
  " x and D only delete, no yank/cut but retain cut behavior for d
    nnoremap d  d
    xnoremap d  d
    vnoremap d  d
    nnoremap dd dd

  Plug 'tommcdo/vim-exchange'
  " Exchange text with repeated cx{motion}

  Plug 'AndrewRadev/sideways.vim'
  " Move things sideways in lists
    nnoremap cl :SidewaysRight<cr>
    nnoremap ch :SidewaysLeft<cr>

  Plug 'tranvansang/vim-close-pair'
  " Manually close pairs
    let g:close_pair_key = '<C-]>'
    imap <C-g>] <C-]><Left>
    imap <C-g>o <C-]><Left><CR><Up><C-o>o

  Plug 'svermeulen/vim-subversive'
  " Substitute from yank
    let g:subversivePreserveCursorPosition = 1
    let g:subversivePromptWithActualCommand = 1
    nmap s <plug>(SubversiveSubstitute)
    nmap ss <plug>(SubversiveSubstituteLine)
    nmap S <plug>(SubversiveSubstituteToEndOfLine)
    " Substitute word under cursor in motion

    let g:which_key_map['s'] = 'subversive-substitute-range'
    let g:which_key_map['ss'] = 'subversive-substitute-word-range'

    nmap <leader>s <plug>(SubversiveSubstituteRange)
    xmap <leader>s <plug>(SubversiveSubstituteRange)
    nmap <leader>ss <plug>(SubversiveSubstituteWordRange)

    " Paste in visual mode
    xmap p <plug>(SubversiveSubstitute)
    xmap P <plug>(SubversiveSubstitute)

  " These are deprecated in favor of vim-sandwich, which does both and more.
  " However, it's a pretty utility we don't want to use/watch break in
  " complicated environments, so in these cases we stick with the classics:
  " Plug 'tpope/vim-surround'                 " ds, cs, ys to change text surroundings
  " Plug 'AndrewRadev/dsf.vim'                " Delete/change surrounding function

  Plug 'machakann/vim-sandwich'
  " Fancier vim-surround + d

    function s:SandwichCallback()
      " Use vim-surround bindings
      runtime macros/sandwich/keymap/surround.vim

      if !exists('g:sandwich#recipes')
        echom 'Sandwich does not appear to be installed, skipping hook.'
        return
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

      " We also add text object targets for interacting with inner surroundings:
      xmap is   <Plug>(textobj-sandwich-query-i)
      xmap as   <Plug>(textobj-sandwich-query-a)
      omap is   <Plug>(textobj-sandwich-query-i)
      omap as   <Plug>(textobj-sandwich-query-a)
      xmap iss  <Plug>(textobj-sandwich-auto-i)
      xmap ass  <Plug>(textobj-sandwich-auto-a)
      omap iss  <Plug>(textobj-sandwich-auto-i)
      omap ass  <Plug>(textobj-sandwich-auto-a)
      xmap im   <Plug>(textobj-sandwich-literal-query-i)
      xmap am   <Plug>(textobj-sandwich-literal-query-a)
      omap im   <Plug>(textobj-sandwich-literal-query-i)
      omap am   <Plug>(textobj-sandwich-literal-query-a)

      augroup SandwichFiletypes
        autocmd!
        autocmd FileType python call sandwich#util#addlocal([
          \   {'buns': ['"""', '"""'], 'nesting': 0, 'input': ['3"']},
          \ ])

      augroup END
    endfunction

    Plug 'matze/vim-move'

    Plug 'lervag/lists.vim'
      let g:lists_filetypes = ['wiki', 'markdown']

      nmap <leader>wlt <plug>(lists-toggle)
      imap <C-g>lt <plug>(lists-toggle)

    return [function('s:SandwichCallback')]
endfunction

" vim: set ts=2 sw=2 tw=80 et :

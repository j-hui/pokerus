function plugins#editing#setup()
  let l:callbacks = []

  if has('nvim-0.5.0')
    Plug 'numToStr/Comment.nvim'
    function s:SetupCommentNvim()
lua <<EOF
      require'Comment'.setup {}
EOF
    endfunction

    let l:callbacks += [function('s:SetupCommentNvim')]
  else
    Plug 'tpope/vim-commentary'
    " use gcc to comment things out

    Plug 'suy/vim-context-commentstring'
    " comments for embedded syntax
  endif

  Plug 'tpope/vim-unimpaired'
  " ]* and [* mappings

  Plug 'tpope/vim-endwise'
  " Write endings
    " let g:endwise_no_mappings = 1
    " Can mess up other <CR> mappings, especially from completion plugins
    " imap <C-g><CR> <CR><Plug>DiscretionaryEnd
    " call g:WhichKeyIG(['<CR>'], 'endwise-end')

  Plug 'rstacruz/vim-closer'
  " Auto-insert endings on <CR>

  Plug 'tpope/vim-abolish'
  " Smarter subtitutions

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
    call g:WhichKeyIG([']'], 'close-pair')
    imap <C-g>o <C-]><Left><CR><Up><C-o>o
    call g:WhichKeyIG(['o'], 'close-pair-next-line')

  Plug 'svermeulen/vim-subversive'
  " Substitute from yank
    let g:subversivePresrveCursorPosition = 1
    let g:subversivePromptWithActualCommand = 1
    nmap gs <plug>(SubversiveSubstitute)
    nmap gss <plug>(SubversiveSubstituteLine)
    call g:WhichKey('n', ['g', 's'], 'subversive-subs')
    " call g:WhichKey('n', ['g', 's', 's'], 'subversive-subs-line')

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

    function s:SetupSandwich()
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

    let l:callbacks += [function('s:SetupSandwich')]

    Plug 'matze/vim-move'

    Plug 'lervag/lists.vim'
      let g:lists_filetypes = ['wiki', 'markdown']

      nmap <leader>wlt <plug>(lists-toggle)
      imap <C-g>lt <plug>(lists-toggle)

    Plug 'junegunn/goyo.vim'
      let g:goyo_linenr = 1
      nnoremap <leader>G :Goyo<CR>
      call g:WhichKeyL(['G'], 'goyo-toggle')

      function! s:goyo_enter()
        call g:ShareSetMode(1)
      endfunction

      function! s:goyo_leave()
        call g:ShareSetMode(0)
      endfunction

      augroup goyo-settings
        autocmd!
        autocmd User GoyoEnter nested call <SID>goyo_enter()
        autocmd User GoyoLeave nested call <SID>goyo_leave()
      augroup END

    if has('nvim-0.6')
      Plug 'jbyuki/venn.nvim'
      " Draw diagrams using visual mode
      function s:ToggleVenn()
        if exists('g:venn_enabled') && g:venn_enabled
          let g:venn_enabled = v:false
          let &virtualedit = g:venn_ve
          nunmap <Down>
          nunmap <Up>
          nunmap <Left>
          nunmap <Right>
          vunmap <CR>
          echom 'Disabled Venn mode'
        else
          let g:venn_enabled = v:true
          let g:venn_ve = &virtualedit
          setlocal virtualedit=all
          nnoremap <Down>   <C-v>j:VBox<CR>
          nnoremap <Up>     <C-v>k:VBox<CR>
          nnoremap <Left>   <C-v>h:VBox<CR>
          nnoremap <Right>  <C-v>l:VBox<CR>
          vnoremap <CR> :VBox<CR>
          echom 'Enabled Venn mode'
        endif
      endfunction
      command Venn call s:ToggleVenn()
    endif

    return l:callbacks
endfunction

" vim: set ts=2 sw=2 tw=80 et :

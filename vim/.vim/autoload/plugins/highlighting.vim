
function plugins#highlighting#setup()
  Plug 'sainnhe/gruvbox-material'
  Plug 'sainnhe/everforest'       " Gruvbox-like
  Plug 'sainnhe/edge'             " Onedark-like
  Plug 'sainnhe/sonokai'          " Monokai-like
  " sainnhe's themes

    function! s:mod_highlight() abort
      highlight Error    cterm=undercurl gui=undercurl
      highlight ErrorMsg cterm=undercurl gui=undercurl
      highlight ALEError cterm=undercurl gui=undercurl
      highlight SpellBad cterm=undercurl gui=undercurl
      " Darken Pmenu background to avoid clash with Cursorline or ColorColumn
      " highlight Pmenu ctermbg=233
    endfunction

    augroup AddUndercurl
      autocmd!
      autocmd ColorScheme everforest,edge,gruvbox-material,sonokai call s:mod_highlight()
    augroup END

    " let g:gruvbox_material_enable_bold = 1
    let g:gruvbox_material_diagnostic_text_highlight = 1
    let g:gruvbox_material_current_word = 'bold'

  Plug 'guns/xterm-color-table.vim'
  " Preview all 256 xterm colors

  Plug 'ap/vim-css-color'
  " Highlight hex colors

    command! ColorToggle call css_color#toggle()

    function s:CssColorInit(typ, keywords, groups)
      try
        call css_color#init(a:typ, a:keywords, a:groups)
      catch /^Vim\%((\a\+)\)\=:E117/
        " echom 'ap/vim-css-color not yet installed.'
      endtry
    endfunction

    augroup CssColorCustomFiletypes
      autocmd!
      autocmd Filetype conf call s:CssColorInit('hex', 'none', 'confComment,confString')
      autocmd Filetype haskell call s:CssColorInit('hex', 'none', 'haskellLineComment,haskellString,haskellBlockComment')
    augroup END

  Plug 'itchyny/vim-cursorword'
  " Temporary word highlighting/* preview

    let g:cursorword_delay = 369
    let b:cursorword = 1
    function! CursorWordToggleFn()
      if b:cursorword
        let b:cursorword = 0
      else
        let b:cursorword = 1
      endif
    endfunction
    command! ToggleCursorWord call CursorWordToggleFn()

  Plug 'lfv89/vim-interestingwords'
  " Persistent word highlighting
    let g:interestingWordsDefaultMappings = 0
    command! Noh noh | call UncolorAllWords()

    nnoremap <silent> <leader>m :call InterestingWords('n')<cr>
    vnoremap <silent> <leader>m :call InterestingWords('v')<cr>
    nnoremap <silent> <leader>M :Noh <cr>
    let g:which_key_map['m'] = 'mark-interesting'
    let g:which_key_map['M'] = 'unmark-all'

    nnoremap <silent> ]m :call WordNavigation(1)<cr>
    nnoremap <silent> [m :call WordNavigation(0)<cr>

  Plug 'machakann/vim-highlightedyank'
  " Briefly highlight yanked item

    let g:highlightedyank_highlight_duration = 500

    augroup Highlightedyank
      autocmd!
      autocmd ColorScheme * highlight link HighlightedyankRegion CursorLine
    augroup END

  return []
endfunction

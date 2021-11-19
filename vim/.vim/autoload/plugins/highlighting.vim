
function plugins#highlighting#setup()
  let l:callbacks = []

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

      highlight link LspCodeLens Todo
    endfunction

    augroup ModHighlights
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

  Plug 'RRethy/vim-illuminate'
    let g:Illuminate_delay = 69
    let g:Illuminate_highlightUnderCursor = 0
    let g:Illuminate_ftblacklist = ['nerdtree']

  Plug 'lfv89/vim-interestingwords'
  " Persistent word highlighting
    let g:interestingWordsDefaultMappings = 0
    command! Noh noh | call UncolorAllWords()

    nnoremap <silent> <leader>m :call InterestingWords('n')<cr>
    vnoremap <silent> <leader>m :call InterestingWords('v')<cr>
    nnoremap <silent> <leader>M :Noh <cr>

    call g:WhichKeyL(['m'], 'mark-interesting')
    call g:WhichKeyL(['M'], 'unmark-all')

    nnoremap <silent> ]m :call WordNavigation(1)<cr>
    nnoremap <silent> [m :call WordNavigation(0)<cr>

  Plug 'machakann/vim-highlightedyank'
  " Briefly highlight yanked item

    let g:highlightedyank_highlight_duration = 500

    augroup Highlightedyank
      autocmd!
      autocmd ColorScheme * highlight link HighlightedyankRegion CursorLine
    augroup END

  if has('nvim-0.5')
    Plug 'nvim-lua/plenary.nvim'
    Plug 'folke/todo-comments.nvim'
      function s:SetupTodoComments()
lua <<EOF
        require'todo-comments'.setup()
EOF
      endfunction
      let l:callbacks += [function('s:SetupTodoComments')]

    Plug 'kyazdani42/nvim-web-devicons'
      function s:SetupDevIcons()
        lua require'nvim-web-devicons'.setup{}
      endfunction
      let l:callbacks += [function('s:SetupDevIcons')]


    Plug 'folke/lsp-colors.nvim'
      function s:SetupLspColors()
        lua require'lsp-colors'.setup{}
      endfunction
      let l:callbacks += [function('s:SetupLspColors')]

    Plug 'kevinhwang91/nvim-hlslens'
      noremap <silent> n <Cmd>execute('normal! ' . v:count1 . 'n')<CR>
                  \<Cmd>lua require('hlslens').start()<CR>
      noremap <silent> N <Cmd>execute('normal! ' . v:count1 . 'N')<CR>
                  \<Cmd>lua require('hlslens').start()<CR>
      noremap * *<Cmd>lua require('hlslens').start()<CR>
      noremap # #<Cmd>lua require('hlslens').start()<CR>
      noremap g* g*<Cmd>lua require('hlslens').start()<CR>
      noremap g# g#<Cmd>lua require('hlslens').start()<CR>
  endif

  return l:callbacks
endfunction

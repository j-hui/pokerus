" Window appearance
" ----------------------------------------------------------------------------

function plugins#window#setup()
  let l:callbacks = []
  Plug 'ourigen/skyline.vim'
  " Lightweight status line that takes the guesswork out of configuration
    let g:skyline_fugitive = 1
    let g:skyline_wordcount = 1
    let g:skyline_linecount = 1
    let g:skyline_bufnum = 0

  Plug 'ap/vim-buftabline'
  " Tab bar at top
    let g:buftabline_indicators = 1 " Show whether modified
    let g:buftabline_numbers  = 1   " Show buffer numbers

  Plug 'moll/vim-bbye'
  " Delete buffers without messing up buffer layout

  Plug 'AndrewRadev/bufferize.vim'
  " Command contents in buffer

  Plug 'AndrewRadev/linediff.vim'
  " Vimdiff line ranges

  Plug 'Konfekt/FastFold'
  " Lazy folding
    let g:fastfold_fold_movement_commands = []

  if has('nvim-0.5.1')
    Plug 'luukvbaal/stabilize.nvim'
    " Prevent stabilize buffer focus during splits
    function s:SetupStabilize()
      lua require'stabilize'.setup()
    endfunction
    let l:callbacks += [function('s:SetupStabilize')]

    Plug 'petertriho/nvim-scrollbar'
    " Side scrollbar
      augroup scrollbar-highlights
        autocmd!
        autocmd Colorscheme * highlight! link ScrollbarHandle StatusLine
      augroup END
    function s:SetupScrollbar()
lua <<EOF
      require'scrollbar'.setup{}
EOF
    endfunction
    let l:callbacks += [function('s:SetupScrollbar')]
  endif

  return l:callbacks
endfunction

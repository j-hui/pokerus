" Window appearance
" ----------------------------------------------------------------------------

function plugins#window#setup()
  Plug 'itchyny/lightline.vim'
  " Lightweight status line at bottom

    let g:lightline['colorscheme'] = g:lightline_colorscheme

    let g:lightline['active']['left'] = [
      \ [ 'mode', 'paste' ],
      \ [ 'gitbranch', 'readonly', 'relativepath', 'modified' ],
      \ [ 'textwidth', 'formatoptions' ],
      \]
    let g:lightline['active']['right'] = [
      \ [ 'lineinfo' ],
      \ [ 'percent' ],
      \ [ 'scrollbar'],
      \ [ 'spell', 'fileformat', 'fileencoding', 'filetype' ],
      \]

    let g:lightline['component']['scrollbar'] = '%{ScrollStatus()}'
    let g:lightline['component']['formatoptions'] = '%{&formatoptions}'
    let g:lightline['component']['textwidth'] = '%{&textwidth}'

    " NOTE: depends on tpope/vim-fugutive
    let g:lightline['component_function']['gitbranch'] = 'FugitiveHead'

  Plug 'ojroques/vim-scrollstatus'
  " Scroll bar on status line
    let g:scrollstatus_size = 20

  Plug 'ap/vim-buftabline'
  " Tab bar at top
    let g:buftabline_indicators = 1 " Show whether modified
    let g:buftabline_numbers  = 1   " Show buffer numbers

    nmap <C-w>1 <Plug>BufTabLine.Go(1)
    nmap <C-w>2 <Plug>BufTabLine.Go(2)
    nmap <C-w>3 <Plug>BufTabLine.Go(3)
    nmap <C-w>4 <Plug>BufTabLine.Go(4)
    nmap <C-w>5 <Plug>BufTabLine.Go(5)
    nmap <C-w>6 <Plug>BufTabLine.Go(6)
    nmap <C-w>7 <Plug>BufTabLine.Go(7)
    nmap <C-w>8 <Plug>BufTabLine.Go(8)
    nmap <C-w>9 <Plug>BufTabLine.Go(9)
    nmap <C-w>0 <Plug>BufTabLine.Go(-1)

  Plug 'moll/vim-bbye'
  " Delete buffers without messing up buffer layout

  Plug 'AndrewRadev/bufferize.vim'
  " Command contents in buffer

  Plug 'AndrewRadev/linediff.vim'
  " Vimdiff line ranges

  Plug 'Konfekt/FastFold'
  " Lazy folding

  return []
endfunction

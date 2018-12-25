"""""""""""""""""""""""""""""
" jzh2106 VIM Settings
"""""""""""""""""""""""""""""
" Pathogen
execute pathogen#infect()
filetype plugin indent on

" vim-airline
set laststatus=2
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1

" Basic stuff
set nu
set rnu
set wildmenu
"set lazyredraw
set ruler
set pastetoggle=<F2>
syntax enable

" Theme
colorscheme ron
"set background=dark

let &colorcolumn=join(range(81,999),",")
highlight ColorColumn ctermbg=235 guibg=#2c2d27

autocmd InsertEnter * set cul
autocmd InsertLeave * set nocul

 " Whitespace rules
set tabstop=4 expandtab shiftwidth=4 softtabstop=4

"autocmd FileType python setlocal tabstop=4 expandtab shiftwidth=4 softtabstop=4
autocmd FileType c setlocal tabstop=8 noexpandtab shiftwidth=8 softtabstop=8
autocmd FileType make setlocal noexpandtab
autocmd Filetype ocaml setlocal tabstop=2 expandtab shiftwidth=2 softtabstop=2
autocmd Filetype haskell setlocal tabstop=2 expandtab shiftwidth=2 softtabstop=2
autocmd Filetype yaml setlocal tabstop=2 expandtab shiftwidth=2 softtabstop=2
autocmd Filetype go setlocal tabstop=2 expandtab shiftwidth=2 softtabstop=2
autocmd Filetype bib setlocal tabstop=2 expandtab shiftwidth=2 softtabstop=2
autocmd Filetype tex setlocal tabstop=2 expandtab shiftwidth=2 softtabstop=2

autocmd FileType c setlocal commentstring=//\ %s

" Use smart tabs
set smarttab

" Linebreak on 500 characters
set lbr
set tw=500

" Indent
set si " Smart indent
set ai " Auto indent

" Wrap lines
set wrap

" Show matching brackets
set showmatch

" Searching
set hlsearch " highlight search
set incsearch " incremental search
set ignorecase " ignores case
set smartcase " smart case

" Many undo levels
set undolevels=1000

" Folding
set foldlevelstart=10
set foldnestmax=10 
set foldmethod=indent

" Movement
nnoremap j gj
nnoremap k gk
nnoremap <C-j> <C-d>
nnoremap <C-k> <C-u>

inoremap kj <Esc>
vnoremap kj <Esc>

" open files where left off
let g:lastplace_open_folds = 0
"autocmd BufReadPost *
"\ if line("'\"") > 0 && line("'\"") <= line("$") |
"\   exe "nmtoptions=armal! g`\"" |
"\ endif

" Indent-Guides
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
let g:indent_guides_enable_on_vim_startup = 1

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

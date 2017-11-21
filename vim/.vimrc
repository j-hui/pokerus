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
set lazyredraw
set ruler
set pastetoggle=<F2>
syntax enable

" Theme
 colorscheme elflord
 set background=dark

 let &colorcolumn=join(range(81,999),",")
 highlight ColorColumn ctermbg=235 guibg=#2c2d27

 " Whitespace rules
set tabstop=4 expandtab shiftwidth=4 softtabstop=4

"autocmd FileType python setlocal tabstop=4 expandtab shiftwidth=4 softtabstop=4
autocmd FileType c setlocal tabstop=8 noexpandtab shiftwidth=8 softtabstop=8
autocmd FileType make setlocal noexpandtab
autocmd Filetype ocaml setlocal tabstop=2 expandtab shiftwidth=2 softtabstop=2
autocmd Filetype yaml setlocal tabstop=2 expandtab shiftwidth=2 softtabstop=2
inoremap <S-Tab> <C-V><Tab>

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
nnoremap <space> za
set foldmethod=indent

" Movement
nnoremap j gj
nnoremap k gk

" open files where left off
let g:lastplace_open_folds = 0
"autocmd BufReadPost *
"\ if line("'\"") > 0 && line("'\"") <= line("$") |
"\   exe "normal! g`\"" |
"\ endif

" Indent-Guides
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

map ,m :!echo; echo '*** make ***'; echo; make<CR>
map ,t :!echo; echo '*** make test ***'; echo; make test<CR>
map ,r :!echo; echo '*** make run ***'; echo; make run<CR>
map ,c :!echo; echo '*** make clean ***'; echo; make clean<CR>
map ,a :!echo; echo '*** make all ***'; echo; make all<CR>

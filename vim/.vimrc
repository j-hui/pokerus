"""""""""""""""""""""""""""""
" jzh2106 VIM Settings
"""""""""""""""""""""""""""""
" Pathogen
execute pathogen#infect()
filetype plugin indent on

" vim-airline
set laststatus=2
let g:airline#extensions#tabline#enabled = 1
" let g:airline_powerline_fonts = 1

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
"
" Linebreak on 80 characters
set lbr
set textwidth=80

set modeline

setlocal spelllang=en_us
set spellfile=~/.vim/spell/en.utf-8.add

"autocmd FileType python setlocal tabstop=4 expandtab shiftwidth=4 softtabstop=4
autocmd FileType c setlocal tabstop=8 noexpandtab shiftwidth=8 softtabstop=8
autocmd FileType make setlocal noexpandtab
autocmd Filetype ocaml setlocal tabstop=2 expandtab shiftwidth=2 softtabstop=2 commentstring=(*%s*)
autocmd Filetype haskell setlocal tabstop=2 expandtab shiftwidth=2 softtabstop=2
autocmd Filetype yaml setlocal tabstop=2 expandtab shiftwidth=2 softtabstop=2
autocmd Filetype go setlocal tabstop=2 expandtab shiftwidth=2 softtabstop=2
autocmd Filetype bib setlocal tabstop=2 expandtab shiftwidth=2 softtabstop=2

"autocmd FileType c setlocal commentstring=//\ %s

" Use smart tabs
set smarttab


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

" open files where left off
let g:lastplace_open_folds = 0
"autocmd BufReadPost *
"\ if line("'\"") > 0 && line("'\"") <= line("$") |
"\   exe "nmtoptions=armal! g`\"" |
"\ endif

" Indent-Guides
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
" let g:indent_guides_enable_on_vim_startup = 1
" just use <Leader>ig

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

nnoremap <C-n> <C-w>

let t_ZH="\e[3m"
let t_ZR="\e[23m"

hi htmlItalic
            \ term=standout
            \ ctermfg=121
            \ guifg=Green
highlight htmlBoldItalic
            \ term=bold,standout
            \ cterm=bold ctermfg=121
            \ gui=bold guifg=Green
highlight htmlUnderlineItalic
            \ term=underline,standout
            \ cterm=underline ctermfg=121
            \ gui=underline guifg=Green
highlight htmlBoldUnderlineItalic
            \ term=underline,bold,standout
            \ cterm=underline,bold ctermfg=121
            \ gui=underline,bold guifg=Green

""" Begin: LaTeX """

au Filetype tex setlocal
            \ tabstop=2
            \ expandtab
            \ shiftwidth=2
            \ softtabstop=2
            \ textwidth=80
            \ spell
au Filetype tex highlight Conceal ctermfg=NONE ctermbg=NONE
" otherwise rendered symbols render with a weird grey background color

" Vimtex
let g:tex_flavor='latex'
" let g:vimtex_view_method='zathura'
" let g:vimtex_quickfix_mode=0 " wtf this doesn't seem to work
" let g:vimtex_compiler_enabled = 0
"
let g:vimtex_quickfix_open_on_warning=0
let g:vimtex_quickfix_autoclose_after_keystrokes=2
let g:vimtex_quickfix_mode=2  " open on errors without focus

set conceallevel=2
let g:tex_conceal='abdmg'
let g:Tex_GotoError=0


""" End: LaTeX """

""" Begin: Markdown """
au Filetype markdown setlocal
            \ tabstop=4
            \ expandtab
            \ shiftwidth=4
            \ softtabstop=4
            \ textwidth=80
            \ spell

""" End: Markdown """

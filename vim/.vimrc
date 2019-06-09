""""""""""""""""""""""
" j-hui VIM Settings "
""""""""""""""""""""""

"""""""""""""
" Basic stuff
"""""""""""""
" execute pathogen#infect()
call plug#begin('~/.vimplugins/plugged')

" set lazyredraw
filetype plugin indent on

setlocal spelllang=en_us
set spellfile=~/.vim/spell/en.utf-8.add


""""""""""""
" Appearance
""""""""""""
syntax enable
colorscheme ron
set background=dark

set nu
set rnu
set modeline
set ruler
set scrolloff=5
set display+=lastline

" highlight 80-character boundary
let &colorcolumn=join(range(81,999),",")
highlight ColorColumn ctermbg=235 guibg=#2c2d27

" cursor underline in insert mode
autocmd InsertEnter * set cul
autocmd InsertLeave * set nocul

" If terminal supports displaying italics, we need these key sequences
let t_ZH="\e[3m"
let t_ZR="\e[23m"

" Otherwise, we just unugly the italics highlighting
highlight htmlItalic
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

Plug 'vim-airline/vim-airline'
set laststatus=2
let g:airline#extensions#tabline#enabled = 1
" let g:airline_powerline_fonts = 1

Plug 'nathanaelkane/vim-indent-guides'
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_auto_colors = 0
highlight IndentGuidesOdd   guibg=black     ctermbg=black
highlight IndentGuidesEven  guibg=darkgrey  ctermbg=232 " ctermbg 233


""""""""""""
" Navigation
""""""""""""
set undolevels=1000

" Movement
nnoremap j gj
nnoremap k gk
nnoremap <C-j> <C-d>
nnoremap <C-k> <C-u>

inoremap kj <Esc>
" set timeout timeoutlen=200

" Just learn to use <C-[> for <ESC> instead of remapping
" inoremap <C-d> <Esc>

set pastetoggle=<F2>

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

set hlsearch    " highlight search
set incsearch   " incremental search
set ignorecase  " ignores case
set smartcase   " smart case
set showmatch   " Show matching brackets

set foldlevelstart=10
set foldnestmax=10
set foldmethod=indent

set wildmenu

set autoread

Plug 'tpope/vim-repeat'

Plug 'farmergreg/vim-lastplace'
let g:lastplace_open_folds = 0

Plug 'easymotion/vim-easymotion'
let g:EasyMotion_do_mapping = 0
let g:EasyMotion_smartcase = 1
map <C-f>l <Plug>(easymotion-lineforward)
map <C-f>j <Plug>(easymotion-j)
map <C-f>k <Plug>(easymotion-k)
map <C-f>h <Plug>(easymotion-linebackward)
map  <C-f>/ <Plug>(easymotion-sn)
omap <C-f>/ <Plug>(easymotion-tn)
map  <C-f>n <Plug>(easymotion-next)
map  <C-f>N <Plug>(easymotion-prev)

let g:EasyMotion_startofline = 0 " keep cursor column when JK motion

Plug 'scrooloose/nerdtree'
map <C-n> :NERDTreeToggle<CR>
map <Leader>m :NERDTreeToggle<CR>

" Open NERDTree upon startup
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" Open NERDTree when opening a directory
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) &&
            \ !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene |
            \ exe 'cd '.argv()[0] | endif

" Close vim if NERDTree the only window left
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") &&
            \ b:NERDTree.isTabTree()) | q | endif
Plug 'Xuyuanp/nerdtree-git-plugin'

Plug 'airblade/vim-gitgutter'
let g:gitgutter_map_keys = 0
nmap <c-g><c-g> <Plug>GitGutterPreviewHunk
nmap <c-g>n <Plug>GitGutterNextHunk
nmap <c-g>p <Plug>GitGutterPrevHunk
set updatetime=100
let g:gitgutter_override_sign_column_highlight = 0
highlight SignColumn        guibg=#073642 ctermbg=0
highlight GitGutterAdd      guibg=#073642 ctermbg=0 guifg=#009900 ctermfg=2
highlight GitGutterChange   guibg=#073642 ctermbg=0 guifg=#bbbb00 ctermfg=3
highlight GitGutterDelete   guibg=#073642 ctermbg=0 guifg=#ff2222 ctermfg=1

Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'
nmap <c-g>vb :GV
nmap <c-g>vc :GV!
nmap <c-g>vf :GV?

""""""""""""""
" Text editing
""""""""""""""
" Whitespace rules w/ linebreak on 80 characters
set tabstop=4
set expandtab
set softtabstop=4
set shiftwidth=4
set smarttab

set smartindent
set autoindent

" Linebreak on 80 characters
set lbr
set textwidth=80
set wrap

function TrimTrailing()
    substitute/ *$//g
endfunction
cnoreabbrev tt call TrimTrailing()

Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'junegunn/vim-peekaboo'


"""""""
" LaTeX
"""""""
autocmd Filetype tex setlocal
            \ tabstop=2
            \ expandtab
            \ shiftwidth=2
            \ softtabstop=2
            \ textwidth=80
            \ spell

autocmd Filetype tex highlight Conceal ctermfg=NONE ctermbg=NONE
" otherwise rendered symbols render with a weird grey background color

Plug 'lervag/vimtex',   { 'for': 'tex' }
let g:tex_flavor='latex'
" let g:vimtex_view_method='open'
" let g:vimtex_compiler_enabled = 0
" let g:vimtex_quickfix_mode=0 " wtf this doesn't seem to work
let g:vimtex_quickfix_open_on_warning=0
let g:vimtex_quickfix_autoclose_after_keystrokes=2
let g:vimtex_quickfix_mode=2  " open on errors without focus

set conceallevel=2
let g:tex_conceal='abdmg'
let g:Tex_GotoError=0

let g:vimtex_mappings_enabled=0
autocmd Filetype tex imap <C-]> <plug>(vimtex-delim-close)

""""""""""
" Markdown
""""""""""
autocmd Filetype markdown setlocal
            \ tabstop=4
            \ expandtab
            \ shiftwidth=4
            \ softtabstop=4
            \ textwidth=80
            \ spell

"""
" C
"""
autocmd FileType c setlocal
            \ tabstop=8
            \ noexpandtab
            \ shiftwidth=8
            \ softtabstop=8

""""""""""
" Makefile
""""""""""
autocmd FileType make setlocal noexpandtab

"""""""
" OCaml
"""""""
autocmd Filetype ocaml setlocal
            \ tabstop=2
            \ expandtab
            \ shiftwidth=2
            \ softtabstop=2
            \ commentstring=(*%s*)

"""""""""
" Haskell
"""""""""
autocmd Filetype haskell setlocal
            \ tabstop=2
            \ expandtab
            \ shiftwidth=2
            \ softtabstop=2

""""""
" YAML
""""""
autocmd Filetype yaml setlocal
            \ tabstop=2
            \ expandtab
            \ shiftwidth=2
            \ softtabstop=2

""""
" Go
""""
autocmd Filetype go setlocal
            \ tabstop=2
            \ expandtab
            \ shiftwidth=2
            \ softtabstop=2
Plug 'fatih/vim-go',    { 'for': 'go' } "{ 'do': ':GoUpdateBinaries' }

"""""
" Bib
"""""
autocmd Filetype bib setlocal
            \ tabstop=2
            \ expandtab
            \ shiftwidth=2
            \ softtabstop=2


call plug#end()

""""""""""""""""""""""
" j-hui VIM Settings "
""""""""""""""""""""""

"""""""""
" Notes "
"""""""""
"
" Default key bindings that just aren't very useful:
"
" s/S (substitute): delete character/line; insert mode
"   - I can just x/dd i
"
" c/C (change): like d/D, except it drops you into insert mode
"   - I can just d/D i
"
" & : repeat :s
"   - ???
"
" f/F (find): f/F<char> jumps to next/previous occurrence of <char>
"   - Now mapped using sneak.vim
"
" t/T (till): t/T<char> jumps to before next/previous occurence of <char>
"   - actually handy
"
" ; : repeat f/F/t/T
"   - I don't use f/F/t/T
"
" 0/_ (beginning of line): go to hard/soft beginning of line
"   - I already map <c-h> for exactly this
"   - I also have ^ even if it's a little hard to reach
"   + Note: I've remapped _ to D
"
" ^/$ (beginning/end of line): go there
"   - just too far out of reach
"   - I already map <c-h>/<c-l> for exactly this
"
" + (next line): go to next line
"   - just use j
"
" W/E/B (next/end/back WORD): seems to be just like w/e/b
"   - what even is WORD (vs word)
"
" Q (ex mode): go to ex mode
"   - ok but who the fuck uses this
"
""""
"
" Key bindings I should actually use, but require too much thinking
"
" q/@ (record/play macro): q/@<char> records/plays macro at <char>
"
" . (repeat): a little unpredictable imo
"
""""
"
" Key bindings I should actually use, but don't have muscle memory
"
" #/* (prev/next identifier): goto prev/next occurence of token under cursor
"   - too far out of reach
"
" {/} (beginning/end of paragraph): go there
"   - too far out of reach
"
""""


"""""""""""""
" Basic stuff
"""""""""""""
call plug#begin('~/.vimplugins/plugged')

" set lazyredraw
filetype plugin indent on

setlocal spelllang=en_us
set spellfile=~/.vim/spell/en.utf-8.add

" Disable ex mode
nnoremap Q <nop>

function! s:refresh()
  silent! call mkdir(fnamemodify(tempname(), ":p:h"), "", 0700)
  set nohlsearch
  redraw
  redrawstatus
endfunction
command! -bang Refresh call s:refresh()

Plug 'tpope/vim-repeat'

""""""""""""
" Appearance
""""""""""""
set noeb vb t_vb=       " No error bell

set nu                  " Line numbers
set rnu                 " Relative line numbers
set display+=lastline   " Show as much as possible of the last line
set nowrap              " Don't wrap lines
set scrolloff=5         " Keep a few lines under the cursor
set sidescrolloff=4     " Keep a few lines to the side of the cursor
set textwidth=80

augroup cursor_underline
    autocmd!
    autocmd InsertEnter * set cul
    autocmd InsertLeave * set nocul
augroup END

syntax enable
set background=dark

" If terminal supports displaying italics, we need these key sequences
let t_ZH="\e[3m"
let t_ZR="\e[23m"

set colorcolumn=+1,+2
nnoremap <leader>of m`/\%>80v./+<CR>``

augroup color_tweaks
    autocmd!
    autocmd ColorScheme *
        \   highlight clear Conceal
        \|  highlight clear VertSplit
        \|  highlight SignColumn    ctermbg=NONE cterm=NONE guibg=NONE gui=NONE
        \|  highlight ColorColumn   ctermbg=234 guibg=#1c1c1c
        \|  highlight Folded        ctermbg=234 guibg=#1c1c1c
        \|  highlight FoldColumn    ctermbg=234 guibg=#1c1c1c

    autocmd ColorScheme *
        \   highlight Search    cterm=underline,bold ctermfg=blue ctermbg=234
        \                       gui=underline,bold   guifg=blue guibg=#1c1c1c
        \|  highlight IncSearch cterm=underline,bold ctermfg=cyan ctermbg=239
        \                       gui=underline,bold   guifg=cyan guibg=#4e4e4e

    autocmd ColorScheme *
        \   highlight Pmenu         ctermbg=234 ctermfg=white
        \                           guibg=#1c1c1c guifg=white
        \|  highlight PmenuSbar     ctermbg=240 ctermfg=white
        \                           guibg=#585858 guifg=white
        \|  highlight PmenuThumb    ctermbg=240 ctermfg=white
        \                           guibg=#585858 guifg=white
        \|  highlight PmenuSel      ctermbg=240 ctermfg=white cterm=bold
        \                           guibg=#585858 guifg=white gui=bold
        \|  highlight TabLineSel    ctermbg=240 ctermfg=white
        \                           guibg=#1c1c1c guifg=white
        \|  highlight TabLine       ctermbg=234 ctermfg=240
        \                           guibg=#1c1c1c guifg=#585858
        \|  highlight TabLineFill   ctermfg=234
        \                           guibg=#1c1c1c

    autocmd ColorScheme *
        \   highlight htmlItalic                term=standout
        \                                       ctermfg=121
        \                                       guifg=Green
        \|  highlight htmlBoldItalic            term=bold,standout
        \                                       cterm=bold ctermfg=121
        \                                       gui=bold guifg=Green
        \|  highlight htmlUnderlineItalic       term=underline,standout
        \                                       cterm=underline ctermfg=121
        \                                       gui=underline guifg=Green
        \|  highlight htmlBoldUnderlineItalic   term=underline,bold,standout
        \                                       cterm=underline,bold ctermfg=121
        \                                       gui=underline,bold guifg=Green

    autocmd ColorScheme *
        \   highlight SpellBad      ctermbg=NONE ctermfg=red
        \                           guibg=NONE   guifg=red      gui=undercurl
        \|  highlight SpellRare     ctermbg=NONE ctermfg=yellow
        \                           guibg=NONE   guifg=yellow   gui=undercurl
        \|  highlight SpellCap      ctermbg=NONE ctermfg=cyan
        \                           guibg=NONE   guifg=cyan     gui=undercurl
        \|  highlight SpellLocal    ctermbg=NONE ctermfg=yellow
        \                           guibg=NONE   guifg=yellow   gui=undercurl
    autocmd ColorScheme *
        \   highlight DiffAdd       ctermbg=17      cterm=bold
        \                           guibg=#00005f   gui=bold
        \|  highlight DiffDelete    ctermbg=234     ctermfg=242
        \                           guibg=#1c1c1c   guifg=#6c6c6c
        \|  highlight DiffChange    ctermbg=234
        \                           guibg=#1c1c1c
        \|  highlight DiffText      ctermbg=234     cterm=underline
        \                           guibg=#1c1c1c   gui=undercurl
augroup END

set modeline
set modelines=5
function! AppendModeline()
  let l:modeline = printf(" vim: set ts=%d sw=%d tw=%d %set :",
        \ &tabstop, &shiftwidth, &textwidth, &expandtab ? '' : 'no')
  let l:modeline = substitute(&commentstring, "%s", l:modeline, "")
  call append(line("$"), l:modeline)
endfunction
nnoremap <silent> <Leader>ml :call AppendModeline()<CR>

set list lcs=tab:\┆\ " <-- space
set conceallevel=2

if !exists('g:vscode')
    Plug 'junegunn/goyo.vim',       { 'on': 'Goyo' }
    Plug 'junegunn/limelight.vim',  { 'on': 'Limelight' }
    nnoremap <leader>ll :Limelight!!<CR>

    let g:limelight_conceal_ctermfg = 'gray'
    let g:limelight_conceal_ctermfg = 240
    let g:limelight_conceal_guifg = 'DarkGray'
    let g:limelight_conceal_guifg = '#777777'
    let g:limelight_default_coefficient = 0.7
    let g:limelight_paragraph_span = 1
    let g:limelight_bop = '^\s'         " beginning of paragraph
    let g:limelight_eop = '\ze\n^\s'    " end of paragraph
    let g:limelight_priority = -1       " don't overrule hlsearch
endif

if !exists('g:vscode')
    Plug 'itchyny/lightline.vim'
    let g:lightline = {
        \ 'active': {
        \   'left': [ [ 'mode', 'paste' ],
        \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
        \ },
        \ 'component_function': {
        \   'gitbranch': 'FugitiveHead'
        \ },
    \ }

    Plug 'ap/vim-buftabline'
    let g:buftabline_indicators = 1
    let g:buftabline_numbers = 2

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
endif

""""""""""""
" Navigation
""""""""""""
" allow backspacing over everything in insert mode
set backspace=indent,eol,start

set hlsearch    " highlight search
set noincsearch " no incremental search -- to unpredictable
set ignorecase  " ignores case
set smartcase   " smart case
set showmatch   " Show matching brackets

set foldlevelstart=10
set foldnestmax=10
set foldmethod=manual

set wildmenu

set autoread

" Movement
nnoremap j gj
nnoremap k gk
" nnoremap <C-j> <C-d>
" nnoremap <C-k> <C-u>
nnoremap <C-n> <C-e>j
nnoremap <C-p> <C-y>k
nnoremap <C-e> g$
nnoremap <C-a> g^
nnoremap <C-f> l
nnoremap <C-b> h
" nnoremap <C-l> g$ " bad habits xD
" nnoremap <C-h> g^ " bad habits xD

inoremap <C-c> <Esc>
inoremap kj <Esc>
inoremap <C-down> <C-n>
inoremap <C-up> <C-p>
inoremap <C-n> <down>
inoremap <C-p> <up>

noremap <C-w>n <Esc>:bn<CR>
noremap <C-w>p <Esc>:bp<CR>
noremap <C-w>q <Esc>:bd<CR>

Plug 'andymass/vim-matchup'
augroup matchup_matchparen_highlight
  autocmd!
  autocmd ColorScheme * highlight MatchParen guifg=red
augroup END

Plug 'justinmk/vim-sneak'
let g:sneak#label = 1

map f <Plug>Sneak_s
map F <Plug>Sneak_S

Plug 'junegunn/vim-after-object'        " motions for moving after characters
augroup vim_after_hook
    autocmd VimEnter * call after_object#enable('=', ':', '-', '#', ' ')
    " e.g. ya= yanks after first '='; daa= deletes after second '='
augroup END

if !exists('g:vscode')
    Plug 'psliwka/vim-smoothie'
    let g:smoothie_base_speed = 42
    " nnoremap <silent> <C-j>      :<C-U>call smoothie#downwards() <CR>
    " nnoremap <silent> <C-k>      :<C-U>call smoothie#upwards()   <CR>

    Plug 'itchyny/vim-cursorword'
    let g:cursorword_delay = 369
    let b:cursorword = 1
    function! ToggleCursorWord()
        if b:cursorword
            let b:cursorword = 0
        else
            let b:cursorword = 1
        endif
    endfunction
    cnoreabbrev csw call ToggleCursorWord()
endif

if !exists('g:vscode')
    Plug 'farmergreg/vim-lastplace'
    let g:lastplace_open_folds = 0

    Plug 'szw/vim-maximizer'
    let g:maximizer_set_default_mapping = 0
    nnoremap <silent><C-w>f :MaximizerToggle<CR>
    vnoremap <silent><C-w>f :MaximizerToggle<CR>gv
endif

if !exists('g:vscode')
    Plug 'duggiefresh/vim-easydir'

    Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
    noremap <C-w><space> :NERDTreeToggle<CR>

    Plug 'Xuyuanp/nerdtree-git-plugin'
endif

if !exists('g:vscode')
    set updatetime=100

    if has('nvim') || has('patch-8.0.902')
        Plug 'mhinz/vim-signify'
    else
        Plug 'mhinz/vim-signify', { 'branch': 'legacy' }
    endif

    nnoremap <leader>gh :SignifyToggleHighlight<CR>
    nnoremap <leader>gf :SignifyFold!<CR>
    nnoremap <leader>gd :SignifyHunkDiff<CR>
    nnoremap <leader>gu :SignifyHunkUndo<CR>

    Plug 'tpope/vim-fugitive'
    Plug 'junegunn/gv.vim'
endif

if !exists('g:vscode')
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
    Plug 'junegunn/fzf.vim'
    let g:fzf_preview_window = 'right:60%'
    nnoremap <c-w><enter> :Files<CR>

    Plug 'mileszs/ack.vim'
    if executable('ag')
      let g:ackprg = 'ag --vimgrep'
    endif

    nnoremap K :Ack! "\b<C-R><C-W>\b" % <CR>

    nnoremap <Leader>ag :Ack!<Space>
    vnoremap <Leader>ag <Esc>:Ack!<Space>

    nnoremap <Leader>aa :AckAdd!<Space>
    vnoremap <Leader>aa <Esc>:AckAdd!<Space>
endif

if !exists('g:vscode')
    Plug 'voldikss/vim-floaterm'

    nnoremap <silent> <C-_> :FloatermToggle<CR>
    tnoremap <silent> <C-_> <C-\><C-n>:FloatermToggle<CR>

    nnoremap <silent> <F9>  :FloatermNew<CR>
    tnoremap <silent> <F9>  <C-\><C-n>:FloatermNew<CR>

    nnoremap <silent> <F10> :FloatermPrev<CR>
    tnoremap <silent> <F10> <C-\><C-n>:FloatermPrev<CR>

    nnoremap <silent> <F11> :FloatermNext<CR>
    tnoremap <silent> <F11> <C-\><C-n>:FloatermNext<CR>

    nnoremap <silent> <F12> :FloatermToggle<CR>
    tnoremap <silent> <F12> <C-\><C-n>:FloatermToggle<CR>

    tnoremap <silent> <C-[> <C-\><C-n>
endif

""""""""""""""
" Text editing
""""""""""""""
set pastetoggle=<F2>

if system('uname -s') == "Darwin\n"
  "OSX
  set clipboard=unnamed
else
  "Linux
  set clipboard=unnamedplus
endif

" set clipboard+=unnamed

" Whitespace rules w/ linebreak on 80 characters
set expandtab           " Expand tabs to spaces
set tabstop=4           " Expan tabs to 4 spaces
set shiftwidth=0        " Use tabstop value for (auto)indent
set smarttab            " Apply tabs in front of a line according to shiftwidth
set autoindent          " Automatically indent when starting a new line
set nojoinspaces        " Only insert single space after J

xnoremap < <gv
xnoremap > >gv

inoremap # X#
nnoremap <leader>d :put =strftime(\"%Y-%m-%d\")<CR>
inoremap <C-G>d <C-R>=strftime("%Y-%m-%d")<CR>
inoremap <C-G><TAB> <C-F>

command! -range TT <line1>,<line2> substitute/\s\+$//g

if !exists('g:vscode')
    " Deliberately avoid using /tmp/ to avoid leaking data on shared computer
    "
    " To allow backup files to be stored locally, run:
    "
    "       mkdir -p .backup .swp .undo
    "
    " To enable backups to be stashed centrally, put the following in .bashrc
    "
    "       mkdir -p ~/.tmp/backup ~/.tmp/swp ~/.tmp/undo
    "
    " Fallback to using current directory . if all else fails
    "
    " Also, put the following in global .gitignore
    "
    "       *~
    "       *.swp

    " TODO: backups don't work well on nvim with symlinked files
    if !has('nvim')
        set backup
        set backupdir=.backup,~/.tmp/backup//,.
    endif

    set swapfile
    set directory=.swp,~/.tmp/swp//,.

    set undofile
    set undodir=.undo,~/.tmp/undo//,.
    set undolevels=1000
    if has('persistent_undo')
        set undofile
        set undoreload=10000
    endif

    Plug 'mbbill/undotree'
    nnoremap <C-w>u :UndotreeToggle<cr>:UndotreeFocus<cr>
endif

Plug 'svermeulen/vim-cutlass'
" - retain cut behavior for d specifically
" - x and D no longer put text into registers
" - to delete lines without cutting, use D and visual mode
nnoremap d  d
xnoremap d  d
vnoremap d  d
nnoremap dd dd

Plug 'tpope/vim-commentary'         " use gcc to comment things out
Plug 'tpope/vim-surround'           " ds, cs, ys to change text surroundings
Plug 'tpope/vim-characterize'       " use ga to see metadata about unicode
Plug 'tpope/vim-endwise'            " write endings
Plug 'tpope/vim-speeddating'        " increment/decrement dates

let g:speeddating_no_mappings = 1
" We need to do this to force a non-recursive map to the fallback functions
nnoremap <Plug>SpeedDatingFallbackUp    <C-A>
nnoremap <Plug>SpeedDatingFallbackDown  <C-X>
xnoremap <Plug>SpeedDatingFallbackUp    <C-A>
xnoremap <Plug>SpeedDatingFallbackDown  <C-X>

nmap  <C-S>     <Plug>SpeedDatingUp
nmap  <C-X>     <Plug>SpeedDatingDown
xmap  <C-S>     <Plug>SpeedDatingUp
xmap  <C-X>     <Plug>SpeedDatingDown

Plug 'junegunn/vim-easy-align'
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

Plug 'tommcdo/vim-exchange'         " exchange text
Plug 'AndrewRadev/sideways.vim'     " move things sideways in lists
nnoremap <c-g>l :SidewaysRight<cr>
nnoremap <c-g>h :SidewaysLeft<cr>

Plug 'matze/vim-move'               " move things
let g:move_key_modifier = 'C'

if !exists('g:vscode')
    Plug 'junegunn/vim-peekaboo'    " shows yank buffers
    Plug 'tpope/vim-eunuch'         " UNIX-like functionality in Vim
    Plug 'tpope/vim-rsi'            " readline style commands in insert mode
    inoremap <C-D> <C-D>
endif

""""""
" Help
""""""

autocmd FileType help
  \ noremap <buffer><nowait> q :q<CR>

"""""""
" LaTeX
"""""""
augroup latex_settings
    autocmd!
    autocmd Filetype tex setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
                \ textwidth=80
                \ spell
augroup latex_settings

Plug 'lervag/vimtex',   { 'for': 'tex' }
let g:tex_flavor='latex'
" let g:vimtex_view_method='open'
" let g:vimtex_compiler_enabled = 0
" let g:vimtex_quickfix_open_on_warning=0
" let g:vimtex_quickfix_autoclose_after_keystrokes=2
let g:vimtex_quickfix_latexlog = {
    \ 'overfull' : 0,
    \ 'underfull' : 0,
    \ 'packages' : {
    \   'default' : 0,
    \ },
    \}

let g:tex_conceal='abdmg'
let g:Tex_GotoError=0
let g:vimtex_mappings_enabled=0
augroup vimtex_settings
    autocmd!
    autocmd Filetype tex imap <C-]> <plug>(vimtex-delim-close)
augroup END

"""""
" Bib
"""""
augroup bib_settings
    autocmd!
    autocmd Filetype bib setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
augroup END

""""""""""
" Markdown
""""""""""
augroup markdown_settings
    autocmd!
    autocmd Filetype markdown setlocal
                \ tabstop=4
                \ expandtab
                \ shiftwidth=4
                \ softtabstop=4
                \ textwidth=80
                \ spell
augroup END

Plug 'tpope/vim-markdown',  { 'for': 'markdown' }

let g:markdown_fenced_languages = [
            \ 'html',
            \ 'python',
            \ 'bash=sh',
            \ 'c',
            \ 'cpp',
            \ 'ocaml',
            \ 'haskell'
            \ ]

"""
" C
"""
augroup c_settings
    autocmd!
    autocmd BufNewFile,BufReadPost *.c set filetype=c
    autocmd BufNewFile,BufReadPost *.h set filetype=c
    autocmd FileType c setlocal
                \ tabstop=8
                \ noexpandtab
                \ shiftwidth=8
                \ softtabstop=8
augroup END

""""""""""
" Makefile
""""""""""
augroup make_settings
    autocmd!
    autocmd FileType make setlocal noexpandtab
augroup END

"""""""
" OCaml
"""""""
augroup ocaml_settings
    autocmd!
    autocmd Filetype ocaml setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
                \ commentstring=(*%s*)
augroup END

"""""""""
" Haskell
"""""""""
augroup haskell_settings
    autocmd!
    autocmd Filetype haskell setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
augroup END

""""""
" YAML
""""""
augroup yaml_settings
    autocmd!
    autocmd Filetype yaml setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
augroup END

"""""""""""
" Javscript
"""""""""""
augroup javascript_settings
    autocmd!
    autocmd Filetype javascript setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
augroup END

""""
" Go
""""
augroup go_settings
    autocmd!
    autocmd Filetype go setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
augroup END

Plug 'fatih/vim-go',    { 'for': 'go' } "{ 'do': ':GoUpdateBinaries' }

"""""
" Coq
"""""
augroup coq_settings
    autocmd!
    autocmd BufNewFile,BufReadPost *.v set filetype=coq
    autocmd Filetype coq setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
                \ commentstring=(*%s*)
                \ formatoptions=cqort
                \ comments=sr:(*,mb:*,ex:*)
augroup END

Plug 'let-def/vimbufsync', { 'for': 'coq' }
Plug 'whonore/coqtail', { 'for': 'coq' }

if !has('nvim')
augroup coqtail_mappings
    autocmd!

    autocmd Filetype coq
        \   nmap <buffer> <c-c>x        :CoqStart<CR>
        \|  nmap <buffer> <c-c>z        :CoqStop<CR>

    autocmd Filetype coq
        \   nnoremap <buffer> <c-c>.             :CoqToLine<CR>
        \|  inoremap <buffer> <c-c>.        <Esc>:CoqToLine<CR>
        \|  nnoremap <buffer> <c-c>l             m`$:CoqToLine<CR>``
        \|  inoremap <buffer> <c-c>l        <Esc>m`$:CoqToLine<CR>``
        \|  nnoremap <buffer> <c-c><CR>          m`$:CoqToLine<CR>``
        \|  inoremap <buffer> <c-c><CR>     <Esc>m`$:CoqToLine<CR>``

    autocmd Filetype coq
        \   nmap <buffer> <c-c>j                 :CoqNext<CR>
        \|  nmap <buffer> <c-c>k                 :CoqUndo<CR>
        \|  nmap <buffer> <c-c>h                 :CoqJumpToEnd<CR>
        \|  nmap <buffer> <c-c><space>           :CoqGotoGoal!<CR>

    autocmd Filetype coq
        \   nmap <buffer> <c-c>c    <leader>ch  " :Coq Check
        \|  nmap <buffer> <c-c>a    <leader>ca  " :Coq About
        \|  nmap <buffer> <c-c>p    <leader>cp  " :Coq Print
        \|  nmap <buffer> <c-c>n    <leader>cf  " :Coq Locate
        \|  nmap <buffer> <c-c>s    <leader>cs  " :Coq Search

augroup END
else
    let g:coqtail_nomap = 1
endif

""""""
" Lean
""""""
" if !exists('g:vscode')
Plug 'leanprover/lean.vim', { 'for': 'lean' }
" endif

augroup lean_settings
    autocmd!
    autocmd BufNewFile,BufReadPost *.lean set filetype=lean
    autocmd Filetype lean setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
                \ commentstring=--\ %s
                \ formatoptions=cqort
                \ comments=s1fl:/-,mb:-,ex:-/,:--
augroup END

"""""""
" Idris
"""""""

Plug 'idris-hackers/idris-vim', { 'for': 'idris' }

"""""""
" Shell
"""""""

Plug 'z0mbix/vim-shfmt', { 'for': 'sh' }

if filereadable(expand("~/.vim_local"))
    source ~/.vim_local
endif

call plug#end()

colorscheme ron " load this last to trigger autocmds

" vim: set ts=4 sw=4 tw=80 et :

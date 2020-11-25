" ============================================================================
" Pokerus .vimrc (by J-Hui)
" ============================================================================

" <leader> prefixes:
" - <backspace>/<return>: toggle folds
" - i: insert
" - f: Find (FZF)
" - c: Coq (coqtail)

" ============================================================================
" Core/plumbing/hacks {{{
" ============================================================================

" let mapleader      = ' '
" let maplocalleader = ' '

" Note: these are already turned on by vim-plug
set nocompatible
filetype plugin on
syntax on

" Input {{{
" ----------------------------------------------------------------------------
set mouse=a             " Mouse interaction

" Disable ex mode
nnoremap Q <nop>
" }}}

" Output {{{
" ----------------------------------------------------------------------------
set noeb vb t_vb=       " No error bell

" If terminal supports displaying italics, we need these key sequences
let t_ZH="\e[3m"
let t_ZR="\e[23m"
set updatetime=100

" Print undercurl
let &t_Cs = "\e[4:3m"
let &t_Ce = "\e[4:0m"

" Disable background color erase
let &t_ut=''

set lazyredraw
" }}}

" Clipboard {{{
" ----------------------------------------------------------------------------
if system('uname -s') == "Darwin\n"
  "OSX
  set clipboard=unnamed
else
  "Linux
  set clipboard=unnamedplus
endif
" }}}

" Backup {{{
" ----------------------------------------------------------------------------
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
endif

" }}}

" Debugging vim {{{
" ----------------------------------------------------------------------------
" Syntax group underneath cursor
map <F8>
    \ :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
    \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
    \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
" }}}


" }}}

" ============================================================================
" Plugins {{{
" ============================================================================
call plug#begin('~/.vimplugins/plugged')

" Text highlighting {{{
" ----------------------------------------------------------------------------
if !exists('g:vscode')

    Plug 'NLKNguyen/papercolor-theme'
        let s:paper_color_default = {
            \   'theme': {
            \     'default.dark': {
            \       'override' : {
            \         'folded_bg': ['#1c1c1c', '234'],
            \       }
            \     }
            \   }
            \ }
        let s:paper_color_transparent = {
            \   'theme': {
            \     'default.dark': {
            \       'override' : {
            \         'color00'       : ['#000000', '0'],
            \         'linenumber_bg' : ['#000000', '0'],
            \         'diffadd_bg'    : ['#000000', '0'],
            \         'diffdelete_bg' : ['#000000', '0'],
            \         'difftext_bg'   : ['#000000', '0'],
            \         'diffchange_bg' : ['#000000', '0'],
            \         'folded_bg'     : ['#1c1c1c', '234'],
            \         'folded_fg'     : ['#d7875f', '173'],
            \       }
            \     }
            \   }
            \ }
        let g:PaperColor_Theme_Options = s:paper_color_default
        let s:paper_color_transparent_background = 0
        function! PaperColorToggleBackground()
            if s:paper_color_transparent_background
                let g:PaperColor_Theme_Options = s:paper_color_default
                let s:paper_color_transparent_background = 0
            else
                let g:PaperColor_Theme_Options = s:paper_color_transparent
                let s:paper_color_transparent_background = 1
            endif
        endfunction
        " Some terminals have trouble rendering the full background,
        " and PaperColor's 'transparent_background' option doesn't handle
        " removing background colors from other elements.
        command! Bg call PaperColorToggleBackground() | colo PaperColor

    Plug 'ajmwagar/vim-deus'
    Plug 'danilo-augusto/vim-afterglow'
    Plug 'kristijanhusak/vim-hybrid-material'
        let g:enable_bold_font = 1
        let g:hybrid_transparent_background = 1

    Plug 'itchyny/vim-cursorword'           " Unintrusive * preview
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
" }}}

" Window appearance {{{
" ----------------------------------------------------------------------------
if !exists('g:vscode')

    Plug 'itchyny/lightline.vim'        " Lightweight status line at bottom
        let g:lightline = {
            \ 'colorscheme': 'PaperColor',
            \ 'active': {
            \   'left': [
            \       [ 'mode', 'paste' ],
            \       [ 'gitbranch', 'readonly', 'relativepath', 'modified' ],
            \   ],
            \   'right': [
            \       [ 'lineinfo' ],
            \       [ 'percent' ],
            \       [ 'scrollbar'],
            \       [ 'fileformat', 'fileencoding', 'filetype' ],
            \   ],
            \ },
            \ 'component': {
            \   'scrollbar': '%{ScrollStatus()}',
            \ },
            \ 'component_function': {
            \   'gitbranch': 'FugitiveHead',
            \ },
        \ }
        " NOTE: gitbranch componenet depends on tpope/vim-fugutive

    Plug 'ojroques/vim-scrollstatus'    " Scroll bar on status line
        let g:scrollstatus_size = 20

    Plug 'ap/vim-buftabline'            " Tab bar at top
        let g:buftabline_indicators = 1 " Show whether modified
        let g:buftabline_numbers    = 1 " Show buffer numbers

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

    Plug 'szw/vim-maximizer'            " Maximize window
        let g:maximizer_set_default_mapping = 0
        nnoremap <silent><C-w><return> :MaximizerToggle<CR>
        vnoremap <silent><C-w><return> :MaximizerToggle<CR>gv

    Plug 'psliwka/vim-smoothie'         " Scroll acceleration animation
        let g:smoothie_base_speed = 42

endif " vscode
" }}}

" Interactive subsystems {{{
" ----------------------------------------------------------------------------
if !exists('g:vscode')

    Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' } " See filesystem
    Plug 'Xuyuanp/nerdtree-git-plugin'
        noremap <C-w><space> :NERDTreeToggle<CR>

    Plug 'mbbill/undotree'                                  " See undo history
        nnoremap <C-w>u :UndotreeToggle<cr>:UndotreeFocus<cr>

    Plug 'junegunn/fzf.vim'                                 " Fuzzy finder
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
        let g:fzf_preview_window = 'right:60%'
        nnoremap <leader>ff   :Files    <space>
        nnoremap <leader>fe   :Files    <CR>
        nnoremap <leader>fg   :GFiles   <CR>
        nnoremap <leader>fj   :Lines    <CR>
        nnoremap <leader>fa   :Ag       <CR>
        nnoremap <leader>fb   :Buffers  <CR>
        nnoremap <leader>f:   :History: <CR>
        nnoremap <leader>f/   :History/ <CR>

    Plug 'Avi-D-coder/fzf-wordnet.vim'    " Dictionary with FZF
        imap <C-S> <Plug>(fzf-complete-wordnet)

    Plug 'junegunn/vim-peekaboo'          " See yank registers

    Plug 'junegunn/gv.vim'                " See Git history

    Plug 'tpope/vim-fugitive'             " Git interaction

    Plug 'AndrewRadev/bufferize.vim'      " command contents in buffer

    Plug 'dense-analysis/ale'             " Asynchronous linting using LSP
        let g:ale_sign_column_always = 1
        let g:ale_lint_delay = 500
        nmap <silent> [a <Plug>(ale_previous_wrap)
        nmap <silent> ]a <Plug>(ale_next_wrap)
        let g:ale_linters = {'rust': ['analyzer']}


endif
" }}}

" File system {{{
" ----------------------------------------------------------------------------
if !exists('g:vscode')
    Plug 'farmergreg/vim-lastplace'

    Plug 'duggiefresh/vim-easydir'

    Plug 'tpope/vim-eunuch'         " UNIX-like functionality in Vim
endif
" }}}

" Utilities {{{
" ----------------------------------------------------------------------------

Plug 'tpope/vim-repeat'                 " User-defined dot-repeatable actions
Plug 'tpope/vim-commentary'             " use gcc to comment things out
Plug 'tpope/vim-surround'               " ds, cs, ys to change text surroundings
Plug 'tpope/vim-characterize'           " use ga to see metadata about unicode
Plug 'tpope/vim-endwise'                " write endings
Plug 'tpope/vim-speeddating'            " increment/decrement dates
    let g:speeddating_no_mappings = 1   " <C-S> to increment
    " force a non-recursive map to the fallback functions
    nnoremap <Plug>SpeedDatingFallbackUp    <C-A>
    nnoremap <Plug>SpeedDatingFallbackDown  <C-X>
    xnoremap <Plug>SpeedDatingFallbackUp    <C-A>
    xnoremap <Plug>SpeedDatingFallbackDown  <C-X>
    nmap  <C-S>     <Plug>SpeedDatingUp
    nmap  <C-X>     <Plug>SpeedDatingDown
    xmap  <C-S>     <Plug>SpeedDatingUp
    xmap  <C-X>     <Plug>SpeedDatingDown

Plug 'andymass/vim-matchup'             " User-defined pairs

Plug 'justinmk/vim-sneak'               " s works like f/t but with two chars
    let g:sneak#label = 1

Plug 'junegunn/vim-after-object'        " motions for moving after chars
    augroup vim_after_hook
        autocmd!
        autocmd VimEnter * call after_object#enable('=', ':', '-', '#', ' ')
        " e.g. ya= yanks after first '='; daa= deletes after second '='
    augroup END

Plug 'junegunn/vim-easy-align'          " Vertically align text by character
    xmap ga <Plug>(EasyAlign)
    nmap ga <Plug>(EasyAlign)
    let g:easy_align_bypass_fold = 1
    let g:easy_align_delimiters = {
        \ '>': { 'pattern': '>>\|=>\|>' },
        \ '\': { 'pattern': '\\' },
        \ '/': {
        \     'pattern': '//\+\|/\*\|\*/',
        \     'delimiter_align': 'l',
        \     'ignore_groups': ['!Comment']
        \   },
        \ ']': {
        \     'pattern':       '\]\zs',
        \     'left_margin':   0,
        \     'right_margin':  1,
        \     'stick_to_left': 0
        \   },
        \ ')': {
        \     'pattern':       ')\zs',
        \     'left_margin':   0,
        \     'right_margin':  1,
        \     'stick_to_left': 0
        \   },
        \ 'f': {
        \     'pattern': ' \(\S\+(\)\@=',
        \     'left_margin': 0,
        \     'right_margin': 0
        \   },
        \ 'd': {
        \     'pattern': ' \ze\S\+\s*[;=]',
        \     'left_margin': 0,
        \     'right_margin': 0
        \   }
        \ }

Plug 'svermeulen/vim-cutlass'       " x and D no longer yank text to registers
                                    " but retain cut behavior for d
    nnoremap d  d
    xnoremap d  d
    vnoremap d  d
    nnoremap dd dd

Plug 'AndrewRadev/dsf.vim'              " Delete surrounding function
Plug 'AndrewRadev/linediff.vim'         " Vimdiff ranges
Plug 'AndrewRadev/sideways.vim'         " Move things sideways in lists
    nnoremap <c-g>l :SidewaysRight<cr>
    nnoremap <c-g>h :SidewaysLeft<cr>

Plug 'matze/vim-move'                   " Move things in visual mode
    vmap <C-j> <Plug>MoveBlockDown
    vmap <C-l> <Plug>MoveBlockRight
    vmap <C-h> <Plug>MoveBlockLeft
    vmap <C-k> <Plug>MoveBlockUp

Plug 'vim-scripts/ReplaceWithRegister'  " Exchange text with register gr{motion}
Plug 'tommcdo/vim-exchange'             " Exchange text with repeated cx{motion}

Plug 'gyim/vim-boxdraw'                 " Draw ASCII text boxes
Plug 'joom/latex-unicoder.vim'          " Useful for 'pretty' Coq/Lean files
    let g:unicoder_cancel_normal = 1
    let g:unicoder_cancel_insert = 1
    let g:unicoder_cancel_visual = 1
    inoremap <C-l> <Esc>:call unicoder#start(1)<CR>

" }}}

" File types {{{
" ----------------------------------------------------------------------------
if !exists('g:vscode')

" TeX/LaTeX {{{
    Plug 'lervag/vimtex',   { 'for': 'tex' }        " TeX/LaTeX
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
" }}}
" Coq {{{
    Plug 'whonore/coqtail', { 'for': 'coq' }
        function! g:CoqtailHighlight()
          hi def CoqtailChecked ctermbg=236
          hi def CoqtailSent    ctermbg=237
        endfunction
        augroup coqtail_mappings
            autocmd!
            autocmd Filetype coq
                \   nmap <buffer> <c-c>x        :CoqStart<CR>
                \|  nmap <buffer> <c-c>z        :CoqStop<CR>
            autocmd Filetype coq
                \   nnoremap <buffer> <c-c>.             :CoqToLine<CR>
                \|  inoremap <buffer> <c-c>.        <Esc>:CoqToLine<CR>
                \|  nnoremap <buffer> <c-c>l             mz$:CoqToLine<CR>`z
                \|  inoremap <buffer> <c-c>l        <Esc>mz$:CoqToLine<CR>`z
                \|  nnoremap <buffer> <c-c><CR>          mz$:CoqToLine<CR>`z
                \|  inoremap <buffer> <c-c><CR>     <Esc>mz$:CoqToLine<CR>`z
            autocmd Filetype coq
                \   nmap <buffer> <c-c>j                 :CoqNext<CR>
                \|  nmap <buffer> <c-c>k                 :CoqUndo<CR>
                \|  nmap <buffer> <c-c>h                 :CoqJumpToEnd<CR>
                \|  nmap <buffer> <c-c><space>           :CoqGotoGoal!<CR>
            autocmd Filetype coq
                \   nmap <buffer> <c-c>c    <leader>ch
                \|  nmap <buffer> <c-c>a    <leader>ca
                \|  nmap <buffer> <c-c>p    <leader>cp
                \|  nmap <buffer> <c-c>n    <leader>cf
                \|  nmap <buffer> <c-c>s    <leader>cs
                " <leader>ch => :Coq Check
                " <leader>ca => :Coq About
                " <leader>cp => :Coq Print
                " <leader>cf => :Coq Locate
                " <leader>cs => :Coq Search
            autocmd Filetype coq syntax sync fromstart
        augroup END
" }}}
" Markdown {{{
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
    Plug 'jtratner/vim-flavored-markdown', { 'for': 'markdown' }
" }}}
" Others {{{
    Plug 'z0mbix/vim-shfmt',        { 'for': 'sh' }
        let g:shfmt_extra_args = '-i 2 -ci -sr'
    Plug 'fatih/vim-go',            { 'for': 'go' }
    Plug 'leanprover/lean.vim',     { 'for': 'lean' }
    Plug 'idris-hackers/idris-vim', { 'for': 'idris' }
    Plug 'LnL7/vim-nix',            { 'for': 'nix' }
    Plug 'vim-scripts/promela.vim', { 'for': 'promela' }
    Plug 'chrisbra/csv.vim',        { 'for': 'csv' }
    Plug 'rust-lang/rust.vim',      { 'for': 'rust' }
" }}}
endif
" }}}


" Local settings
" ----------------------------------------------------------------------------
"  Note: This has to be here, in the Plugins block, because otherwise I can't
"        use Plug
if filereadable(expand("~/.vim_local"))
    source ~/.vim_local
endif

call plug#end()

" }}}

" ============================================================================
" Settings {{{
" ============================================================================

" Appearance {{{
" ----------------------------------------------------------------------------

set background=dark
try
    colorscheme PaperColor
catch /^Vim\%((\a\+)\)\=:E185/
    " deal with it
    echom 'colorscheme PaperColor not installed, run :PlugInstall'
endtry

set nu rnu                  " Line numbers and relative line numbers
set display+=lastline       " Show as much as possible of the last line
set textwidth=80            " Bound lines to 80 characters
set nowrap                  " But don't wrap lines
set scrolloff=5             " Keep a few lines under the cursor
set sidescrolloff=2         " Keep a few lines to the side of the cursor
set statusline=2

augroup cursor_underline    " Underline cursor in insert mode
    autocmd!
    autocmd InsertEnter * set cul
    autocmd InsertLeave * set nocul
augroup END

set colorcolumn=+1,+2

set list listchars=tab:\┆\ ,trail:·,extends:‥
set conceallevel=2

set foldlevelstart=10
set foldnestmax=10
set foldmethod=manual

augroup ron_color_tweaks " {{{
    autocmd!
    " autocmd ColorScheme ron
    "     \   highlight clear Conceal
    "     \|  highlight clear VertSplit
    "     \|  highlight SignColumn    ctermbg=NONE cterm=NONE guibg=NONE gui=NONE
    "     \|  highlight ColorColumn   ctermbg=234 guibg=#1c1c1c
    "     \|  highlight Folded        ctermbg=234 guibg=#1c1c1c
    "     \|  highlight FoldColumn    ctermbg=234 guibg=#1c1c1c
    " autocmd ColorScheme ron
    "     \   highlight Search    cterm=underline,bold ctermfg=blue ctermbg=234
    "     \                       gui=underline,bold   guifg=blue guibg=#1c1c1c
    "     \|  highlight IncSearch cterm=underline,bold ctermfg=cyan ctermbg=239
    "     \                       gui=underline,bold   guifg=cyan guibg=#4e4e4e
    " autocmd ColorScheme ron
    "     \   highlight Pmenu         ctermbg=234 ctermfg=white
    "     \                           guibg=#1c1c1c guifg=white
    "     \|  highlight PmenuSbar     ctermbg=240 ctermfg=white
    "     \                           guibg=#585858 guifg=white
    "     \|  highlight PmenuThumb    ctermbg=240 ctermfg=white
    "     \                           guibg=#585858 guifg=white
    "     \|  highlight PmenuSel      ctermbg=240 ctermfg=white cterm=bold
    "     \                           guibg=#585858 guifg=white gui=bold
    "     \|  highlight TabLineSel    ctermbg=240 ctermfg=white
    "     \                           guibg=#1c1c1c guifg=white
    "     \|  highlight TabLine       ctermbg=234 ctermfg=240
    "     \                           guibg=#1c1c1c guifg=#585858
    "     \|  highlight TabLineFill   ctermfg=234
    "     \                           guibg=#1c1c1c
    " autocmd ColorScheme ron
    "     \   highlight htmlItalic                term=standout
    "     \                                       ctermfg=121
    "     \                                       guifg=Green
    "     \|  highlight htmlBoldItalic            term=bold,standout
    "     \                                       cterm=bold ctermfg=121
    "     \                                       gui=bold guifg=Green
    "     \|  highlight htmlUnderlineItalic       term=underline,standout
    "     \                                       cterm=underline ctermfg=121
    "     \                                       gui=underline guifg=Green
    "     \|  highlight htmlBoldUnderlineItalic   term=underline,bold,standout
    "     \                                       cterm=underline,bold ctermfg=121
    "     \                                       gui=underline,bold guifg=Green
    " autocmd ColorScheme ron
    "     \   highlight SpellBad      ctermbg=NONE ctermfg=red
    "     \                           guibg=NONE   guifg=red      gui=undercurl
    "     \|  highlight SpellRare     ctermbg=NONE ctermfg=yellow
    "     \                           guibg=NONE   guifg=yellow   gui=undercurl
    "     \|  highlight SpellCap      ctermbg=NONE ctermfg=cyan
    "     \                           guibg=NONE   guifg=cyan     gui=undercurl
    "     \|  highlight SpellLocal    ctermbg=NONE ctermfg=yellow
    "     \                           guibg=NONE   guifg=yellow   gui=undercurl
    " autocmd ColorScheme
    "     \   highlight DiffAdd       ctermbg=17      cterm=bold
    "     \                           guibg=#00005f   gui=bold
    "     \|  highlight DiffDelete    ctermbg=234     ctermfg=242
    "     \                           guibg=#1c1c1c   guifg=#6c6c6c
    "     \|  highlight DiffChange    ctermbg=234
    "     \                           guibg=#1c1c1c
    "     \|  highlight DiffText      ctermbg=234     cterm=underline
    "     \                           guibg=#1c1c1c   gui=undercurl
    "   autocmd ColorScheme ron highlight MatchParen guifg=red
augroup END " }}}

" }}}

" Navigation {{{
" ----------------------------------------------------------------------------
set backspace=indent,eol,start  " backspacing over everything in insert mode
set nostartofline               " prevent cursor from jumping to start of line

set showmatch                   " show matching brackets
set virtualedit=block,onemore   " move cursor end of line

set splitright                  " direction of split

set hlsearch                    " highlight search
set incsearch                   " incremental search
set ignorecase                  " ignores case
set smartcase                   " smart case
set wrapscan                    " jump back to top

set wildmenu                    " use wildmenu
set wildmode=longest:full,full  " sane completion interface
set wildignorecase              " ignore case during completion

" Ignore these file patterns
set wildignore+=*.so,*.swp,*.o,*.a
set wildignore+=*.opus,*.flac,.*mp3,*.ogg,*.mp4,*.webm
set wildignore+=*.pdf,*.jpg,*.png,*.jpeg,*.gif
set wildignore+=*.zip,*.gzip,*.bz2,*.tar,*.xz,*.lrzip,*.lrz

let g:netrw_liststyle = 3
let g:netrw_preview = 1


" }}}

" Editing {{{
" ----------------------------------------------------------------------------

set pastetoggle=<F2>

set expandtab           " Expand tabs to spaces
set tabstop=4           " Expan tabs to 4 spaces
set shiftwidth=0        " Use tabstop value for (auto)indent
set smarttab            " Apply tabs in front of a line according to shiftwidth
set autoindent          " Automatically indent when starting a new line
set nojoinspaces        " Only insert single space after J
set formatoptions+=j    " strip comment leader when joining comment lines

" }}}

" }}}

" ============================================================================
" Key bindings {{{
" ============================================================================

" Appearance {{{
" ----------------------------------------------------------------------------
" Highlight characters past 80
nnoremap <space>of m`/\%>80v./+<CR>``

" Folds
nnoremap <space><backspace> zazz
nnoremap <space><return> zizz
" }}}

" Navigation {{{
" ----------------------------------------------------------------------------

" Block-oriented (non-linewise) navigation
nnoremap j gj
nnoremap k gk

" Get out insert mode easily
inoremap <C-c>  <Esc>
inoremap kj     <Esc>

" Normal mode readline style navigation
nnoremap <C-n>      <C-e>j
nnoremap <C-p>      <C-y>k
nnoremap <C-e>      $
nnoremap <C-a>      ^
nnoremap <C-f>      l
nnoremap <C-b>      h

" Virual mode readline style navigation
vnoremap <C-n>      <C-e>j
vnoremap <C-p>      <C-y>k
vnoremap <C-e>      $
vnoremap <C-a>      ^
vnoremap <C-f>      l
vnoremap <C-b>      h

" Window navigation
noremap <C-w>n <Esc>:bn<CR>
noremap <C-w>p <Esc>:bp<CR>
noremap <C-w>q <Esc>:bd<CR>

" }}}

" Editing {{{
" ----------------------------------------------------------------------------

" Don't leave visual mode when indending
xnoremap < <gv
xnoremap > >gv

" " No clue what the hell this does
" inoremap # X#

" Insert date
nnoremap <space>id :put =strftime(\"%Y-%m-%d\")<CR>
inoremap <C-G>d <C-R>=strftime("%Y-%m-%d")<CR>

" Auto indentation
inoremap <C-G><TAB> <C-F>

" Readline-style keybinds adapted from tpope/vim-rsi {{{

inoremap        <C-A> <C-O>^
inoremap   <C-X><C-A> <C-A>
cnoremap        <C-A> <Home>
cnoremap   <C-X><C-A> <C-A>

inoremap <expr> <C-B> getline('.')=~'^\s*$'&&col('.')>strlen(getline('.'))?"0\<Lt>C-D>\<Lt>Esc>kJs":"\<Lt>Left>"
cnoremap        <C-B> <Left>

" inoremap <expr> <C-D> col('.')>strlen(getline('.'))?"\<Lt>C-D>":"\<Lt>Del>"
cnoremap <expr> <C-D> getcmdpos()>strlen(getcmdline())?"\<Lt>C-D>":"\<Lt>Del>"

inoremap <expr> <C-E> col('.')>strlen(getline('.'))<bar><bar>pumvisible()?"\<Lt>C-E>":"\<Lt>End>"

inoremap <expr> <C-F> col('.')>strlen(getline('.'))?"\<Lt>C-F>":"\<Lt>Right>"
cnoremap <expr> <C-F> getcmdpos()>strlen(getcmdline())?&cedit:"\<Lt>Right>"

function! s:ctrl_u()
  if getcmdpos() > 1
    let @- = getcmdline()[:getcmdpos()-2]
  endif
  return "\<C-U>"
endfunction

cnoremap <expr> <C-U> <SID>ctrl_u()
cnoremap        <C-Y> <C-R>-

inoremap        <C-n> <down>
inoremap        <C-p> <up>
inoremap        <C-k> <Esc>lDi

if &encoding ==# 'latin1' && has('gui_running') && !empty(findfile('plugin/sensible.vim', escape(&rtp, ' ')))
  set encoding=utf-8
endif

function! s:MapMeta() abort
  noremap!        <M-b> <S-Left>
  noremap!        <M-f> <S-Right>
  noremap!        <M-d> <C-O>dw
  cnoremap        <M-d> <S-Right><C-W>
  noremap!        <M-n> <Down>
  noremap!        <M-p> <Up>
  noremap!        <M-BS> <C-W>
  noremap!        <M-C-h> <C-W>
endfunction

if has("gui_running") || has('nvim')
  call s:MapMeta()
else
  silent! exe "set <F29>=\<Esc>b"
  silent! exe "set <F30>=\<Esc>f"
  silent! exe "set <F31>=\<Esc>d"
  silent! exe "set <F32>=\<Esc>n"
  silent! exe "set <F33>=\<Esc>p"
  silent! exe "set <F34>=\<Esc>\<C-?>"
  silent! exe "set <F35>=\<Esc>\<C-H>"
  noremap!        <F29> <S-Left>
  noremap!        <F30> <S-Right>
  noremap!        <F31> <C-O>dw
  cnoremap        <F31> <S-Right><C-W>
  noremap!        <F32> <Down>
  noremap!        <F33> <Up>
  noremap!        <F34> <C-W>
  noremap!        <F35> <C-W>
  augroup rsi_gui
    autocmd GUIEnter * call s:MapMeta()
  augroup END
endif

" }}}


" }}}

" Command mode {{{
" ----------------------------------------------------------------------------

" Don't use <Left> and <Right> key for selecting previous/next match
cnoremap <Left> <Space><BS><Left>
cnoremap <Right> <Space><BS><Right>

" Fake page up and page down
cnoremap <c-d> <c-n><c-n><c-n><c-n><c-n><c-n><c-n><c-n><c-n><c-n>
cnoremap <c-u> <c-p><c-p><c-p><c-p><c-p><c-p><c-p><c-p><c-p><c-p>

" 'Step into' wild menu selection
" The backspace apparently necessary to remove ^I artifact
cnoremap <c-g> <Down><BS>

" }}}
" }}}

" ============================================================================
" Commands {{{
" ============================================================================

" Refresh {{{
function! s:refresh()
  silent! call mkdir(fnamemodify(tempname(), ":p:h"), "", 0700)
  set nohlsearch
  redraw
  redrawstatus
endfunction
command! -bang Refresh call s:refresh()
" }}}

" Trim trailing spaces {{{
command! -range Trim <line1>,<line2> substitute/\s\+$//g | normal! ``
" }}}

" Go to location of file {{{
command! Here cd %:h
" }}}


" "Basic" mode: no mouse interaction, line numbers, or sign column {{{
" Useful for copying and pasting from buffers as text
let s:basicmode = 0
function! s:basicToggle()
    if s:basicmode
        set mouse=a nu rnu signcolumn=auto
        let s:basicmode = 0
    else
        set mouse= nonu nornu signcolumn=no
        let s:basicmode = 1
    endif
endfunction
command! BasicToggle call s:basicToggle()
" }}}

" Modeline {{{
function! AppendModeline()
  let l:modeline = printf(" vim: set ts=%d sw=%d tw=%d %set :",
        \ &tabstop, &shiftwidth, &textwidth, &expandtab ? '' : 'no')
  let l:modeline = substitute(&commentstring, "%s", l:modeline, "")
  call append(line("$"), l:modeline)
endfunction

command! Modeline call AppendModeline() | normal! G
" }}}

" Git root {{{
function! s:root()
  let root = systemlist('git rev-parse --show-toplevel')[0]
  if v:shell_error
    echo 'Not in git repo'
  else
    execute 'lcd' root
    echo 'Changed directory to: '.root
  endif
endfunction
command! Root call s:root()
" }}}

" }}}

" ============================================================================
" Spelling {{{
" ============================================================================

setlocal spelllang=en_us
set spellfile=~/.vim/spell/en.utf-8.add

" }}}

" ============================================================================
" File types {{{
" ============================================================================

set modeline
set modelines=5

augroup help_settings " {{{
    autocmd FileType help
      \ noremap <buffer><nowait> q :q<CR>
augroup END " }}}

augroup vim_settings " {{{
    autocmd!
    autocmd BufNewFile,BufReadPost */.vim_local set filetype=vim
    autocmd Filetype vim setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
augroup END " }}}

augroup latex_settings " {{{
    autocmd!
    autocmd Filetype tex setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
                \ textwidth=80
                \ spell
augroup END " }}}

augroup bib_settings " {{{
    autocmd!
    autocmd Filetype bib setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
augroup END " }}}

augroup markdown_settings " {{{
    autocmd!
    autocmd BufNewFile,BufRead *.md,*.markdown setlocal filetype=ghmarkdown
    autocmd Filetype ghmarkdown setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
                \ textwidth=80
                \ spell
augroup END " }}}

augroup c_settings " {{{
    autocmd!
    autocmd BufNewFile,BufReadPost *.c set filetype=c
    autocmd BufNewFile,BufReadPost *.h set filetype=c
    autocmd FileType c setlocal
                \ tabstop=8
                \ noexpandtab
                \ shiftwidth=8
                \ softtabstop=8
augroup END " }}}

augroup make_settings " {{{
    autocmd!
    autocmd FileType make setlocal noexpandtab
augroup END " }}}

augroup ocaml_settings " {{{
    autocmd!
    autocmd Filetype ocaml setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
                \ commentstring=(*%s*)
augroup END " }}}

augroup haskell_settings " {{{
    autocmd!
    autocmd Filetype haskell setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
augroup END " }}}

augroup yaml_settings " {{{
    autocmd!
    autocmd Filetype yaml setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
augroup END " }}}

augroup javascript_settings " {{{
    autocmd!
    autocmd Filetype javascript setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
augroup END " }}}

augroup go_settings " {{{
    autocmd!
    autocmd Filetype go setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
augroup END " }}}

augroup coq_settings " {{{
    autocmd!
    autocmd BufNewFile,BufReadPost *.v set filetype=coq
    autocmd Filetype coq setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
                \ commentstring=(*%s*)
                \ comments=sr:(*,mb:*,ex:*)
                " \ formatoptions=cqtlj
augroup END " }}}

augroup promela_settings " {{{
    autocmd!
    autocmd BufNewFile,BufReadPost *.prom,*.prm,*.promela  setf promela
    autocmd Filetype promela setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
                " \ commentstring=/*%s*/
                " \ comments=sr:/*,mb:*,ex:*/
                " \ formatoptions=cqtlj
augroup END " }}}

augroup protobuf_settings " {{{
    autocmd!
    autocmd Filetype proto setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
                " \ commentstring=/*%s*/
                " \ comments=sr:/*,mb:*,ex:*/
                " \ formatoptions=cqtlj
augroup END " }}}

augroup lean_settings " {{{
    autocmd!
    autocmd BufNewFile,BufReadPost *.lean set filetype=lean
    autocmd Filetype lean setlocal
                \ tabstop=2
                \ expandtab
                \ shiftwidth=2
                \ softtabstop=2
                \ commentstring=--\ %s
                \ comments=s1fl:/-,mb:-,ex:-/,:--
                " \ formatoptions+=cqortj
augroup END " }}}

augroup csv_settings " {{{
    autocmd!
    autocmd BufNewFile,BufReadPost *.csv set filetype=csv
augroup END " }}}

" }}}

" vim: set ts=4 sw=4 tw=80 et foldmethod=marker foldlevel=0:

" ============================================================================
" Pokerus .vimrc (by J-Hui)
" ============================================================================
"
" Global mnemonics:
" - <C-y>*: yank ring
"
" Normal mode mnemonics:
" - <C-l>*: ALE bindings
" - <C-c>*: Coqtail / Vimtex
" - <C-g>*: FZF
" - <C-w>{u,<space>}: Window Undotree / Ranger
"
" Insert mode mnemonics:
" - <C-s>: UltiSnipsExpandTrigger
" - <C-g>{w,u,d,s,%}: Get Word / Unicode / Date
" - <C-g>{s,%}: Auto-pairs surround (Auto-Pairs) / jump to close
" - <C-g><C-g>: Auto-indent
" - <C-l>: correct speLling

" ============================================================================
" Core/plumbing/hacks {{{
" ============================================================================

if 0
 " Change to 1 if doing a minimal .vimrc test
 set nocompatible
 filetype plugin indent on
 syntax enable
 call plug#begin('~/.vimplugins/plugged')
  " Add specific plugins here
 call plug#end()
 finish
endif

" Note: these are already turned on by vim-plug
set nocompatible
filetype plugin on
syntax on

" I want a unified .vimrc for multiple environments, but also don't want to
" load more plugins than an environment can handle. So we conditionally load
" some plugins if there is anything to suggest we are running within some
" non-terminal application:
let s:env_embedded = exists('g:vscode')

" Input {{
" ----------------------------------------------------------------------------
set mouse=a       " Mouse interaction

" Disable ex mode
nnoremap Q <nop>
" }}}

" Output {{{
" ----------------------------------------------------------------------------
set noeb vb t_vb=     " No error bell

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

" F option supresses output when plugins call :echo{,m}
" set shortmess-=F
" }}}

" Clipboard {{{
" ----------------------------------------------------------------------------
let os = substitute(system('uname'), "\n", "", "")
if os == "Darwin"
  set clipboard=unnamed
elseif os == "Linux"
  set clipboard=unnamedplus
endif

if $DISPLAY != ""
  " xclip doesn't seep to work as reliably as xsel
  let g:clipboard = {
      \   'name': 'xsel_override',
      \   'copy': {
      \    '+': 'xsel --input --clipboard',
      \    '*': 'xsel --input --primary',
      \  },
      \   'paste': {
      \    '+': 'xsel --output --clipboard',
      \    '*': 'xsel --output --primary',
      \   },
      \   'cache_enabled': 1,
      \ }
endif
" }}}

" Backup {{{
" ----------------------------------------------------------------------------
if !s:env_embedded
  " Deliberately avoid using /tmp/ to avoid leaking data on shared computer
  "
  " To allow backup files to be stored locally, run:
  "
  "     mkdir -p .backup .swp .undo
  "
  " To enable backups to be stashed centrally, put the following in .bashrc
  "
  "     mkdir -p ~/.tmp/backup ~/.tmp/swp ~/.tmp/undo
  "
  " Fallback to using current directory . if all else fails
  "
  " Also, put the following in global .gitignore
  "
  "     *~
  "     *.swp

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

" Shell {{{
" ----------------------------------------------------------------------------
set shell=bash
" }}}

" Avoid E173 (vim refuses to close with unvisited buffers) {{{
" ----------------------------------------------------------------------------
if argc() > 1
  silent blast " load last buffer
  silent bfirst " switch back to the first
endif
" }}}

" }}}

" ============================================================================
" Plugins {{{
" ============================================================================
let g:plug_callbacks = []
call plug#begin('~/.vimplugins/plugged')

" Text highlighting {{{
" ----------------------------------------------------------------------------
if !s:env_embedded

  Plug 'NLKNguyen/papercolor-theme'
    function s:PaperColorCb()
      try
        " This colorscheme is only available after the plugin is loaded
        colorscheme PaperColor
      catch /^Vim\%((\a\+)\)\=:E185/
        echom "PaperColor theme not yet installed, cannot use as colorscheme."
      endtry
    endfunction
    let g:plug_callbacks += [function("s:PaperColorCb")]

    let s:paper_color_default = {
      \   'theme': {
      \   'default.dark': {
      \     'override' : {
      \     'folded_bg': ['#1c1c1c', '234'],
      \     }
      \   }
      \   }
      \ }
    let s:paper_color_transparent = {
      \   'theme': {
      \   'default.dark': {
      \     'override' : {
      \     'color00'     : ['#000000', '0'],
      \     'linenumber_bg' : ['#000000', '0'],
      \     'diffadd_bg'  : ['#000000', '0'],
      \     'diffdelete_bg' : ['#000000', '0'],
      \     'difftext_bg'   : ['#000000', '0'],
      \     'diffchange_bg' : ['#000000', '0'],
      \     'folded_bg'   : ['#1c1c1c', '234'],
      \     'folded_fg'   : ['#d7875f', '173'],
      \     }
      \   }
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

  Plug 'guns/xterm-color-table.vim'

  Plug 'itchyny/vim-cursorword'           " Unintrusive * preview
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

  Plug 'machakann/vim-highlightedyank'    " Briefly highlight yanked item
    let g:highlightedyank_highlight_duration = 500

    augroup Highlightedyank
      autocmd!
      autocmd ColorScheme *
            \ highlight HighlightedyankRegion ctermbg=236 gui=reverse
      " That needs to be set after the colorscheme is set
    augroup END
endif " !s:env_embedded
" }}}

" Window appearance {{{
" ----------------------------------------------------------------------------
if !s:env_embedded

  Plug 'itchyny/lightline.vim'    " Lightweight status line at bottom
    let g:lightline = {
      \ 'colorscheme': 'PaperColor',
      \ 'active': {
      \   'left': [
      \     [ 'mode', 'paste' ],
      \     [ 'gitbranch', 'readonly', 'relativepath', 'modified' ],
      \   ],
      \   'right': [
      \     [ 'lineinfo' ],
      \     [ 'percent' ],
      \     [ 'scrollbar'],
      \     [ 'fileformat', 'fileencoding', 'filetype' ],
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

  Plug 'ojroques/vim-scrollstatus'  " Scroll bar on status line
    let g:scrollstatus_size = 20

  Plug 'ap/vim-buftabline'      " Tab bar at top
    let g:buftabline_indicators = 1 " Show whether modified
    let g:buftabline_numbers  = 1 " Show buffer numbers

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

  Plug 'szw/vim-maximizer'      " Maximize window
    let g:maximizer_set_default_mapping = 0
    nnoremap <silent><C-w>m :MaximizerToggle<CR>
    vnoremap <silent><C-w>m :MaximizerToggle<CR>gv

  Plug 'psliwka/vim-smoothie'     " Scroll acceleration animation
    " Note that <C-d> and <C-u> are remapped (among others)
    let g:smoothie_no_default_mappings = 0
    " Refresh rate (default 20, lower is smoother):
    " let g:smoothie_update_interval = 20
    " Beginning of animation (default 10, higher is faster):
    let g:smoothie_speed_linear_factor = 30
    " End of animation (default 10, higher is faster):
    let g:smoothie_speed_constant_factor = 30

endif " !s:env_embedded
" }}}

" Interactive subsystems {{{
" ----------------------------------------------------------------------------
if !s:env_embedded
  Plug 'mbbill/undotree'                        " See undo history
    nnoremap <C-w>u :UndotreeToggle<cr>:UndotreeFocus<cr>

  Plug 'junegunn/fzf.vim'                       " Fuzzy finder
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
    let g:fzf_preview_window = 'right:60%'

    " Insert mode completion
    imap <c-x><c-k> <plug>(fzf-complete-word)
    imap <c-x><c-f> <plug>(fzf-complete-path)
    imap <c-x><c-l> <plug>(fzf-complete-line)

    " function! s:build_quickfix_list(lines)
    "   call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
    "   copen
    "   cc
    " endfunction
    " let g:fzf_action = {
    "   \ 'ctrl-q': function('s:build_quickfix_list'),
    "   \ 'ctrl-t': 'tab split',
    "   \ 'ctrl-x': 'split',
    "   \ 'ctrl-v': 'vsplit'
    "   \ }

  Plug 'https://gitlab.com/mcepl/vim-fzfspell.git'    " FZF for z=

  Plug 'junegunn/vim-peekaboo'                  " See yank registers

  Plug 'junegunn/gv.vim'                        " See Git history

  Plug 'tpope/vim-fugitive'                     " Git interaction

  Plug 'pelodelfuego/vim-swoop'                 " Fast find and replace
    let g:swoopUseDefaultKeyMap = 0             " Just invoke :Swoop

  Plug 'cosminadrianpopescu/vim-tail'           " Make vim behave like tail -f
  Plug 'itchyny/calendar.vim'                   " Calendar app in Vim

  if has('nvim')
    " Only load heavier, asynchronous plugins in nvim. Keep vim light.

    Plug 'dense-analysis/ale'                   " Asynchronous linting using LSP
      let g:ale_sign_column_always = 1
      let g:ale_lint_delay = 500
      let g:ale_echo_msg_error_str = 'Err'
      let g:ale_echo_msg_warning_str = 'Warn'
      let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
      let g:ale_linters = {
            \ 'rust': ['analyzer', 'cargo', 'rustc'],
            \ 'tex': ['proselint', 'chktex'],
            \ 'markdown': ['proselint', 'mdl'],
            \}
      let g:ale_fixers = {
            \ 'rust': ['rustfmt'],
            \ 'bib': ['bibclean'],
            \ '*': ['trim_whitespace', 'remove_trailing_lines'],
            \}
      let g:ale_tex_chktex_options = '-n1 -n36 -n26'

      " This way I can do Al<tab> for autocompletion without remembering to capitalize the 'L'
      command! AleInfo ALEInfo
      command! Aleinfo ALEInfo
      " Funnily enough, this makes something like :Ale work too

      " Note that [a clashes with vim-unimpaired, but it knows to stay out of my way (:
      nmap <silent> [a <Plug>(ale_previous_wrap)
      nmap <silent> ]a <Plug>(ale_next_wrap)

      nmap <silent> <C-l>q      <Plug>(ale_toggle)
      nmap <silent> <C-l><C-]>  <Plug>(ale_go_to_definition)
      nmap <silent> <C-l>gg     <Plug>(ale_go_to_definition)
      nmap <silent> <C-l>gs     <Plug>(ale_go_to_definition_in_split)
      nmap <silent> <C-l>gv     <Plug>(ale_go_to_definition_in_vsplit)

      nmap <silent> <C-l>ff     <Plug>(ale_find_references)
      nmap <silent> <C-l>fr     :ALERepeatSelection<CR>
      nmap <silent> <C-l>fs     :ALEFindReferences -split<CR>
      nmap <silent> <C-l>fv     :ALEFindReferences -vsplit<CR>

      nmap <silent> <C-l>c      <Plug>(ale_hover)
      nmap <silent> <C-l>x      <Plug>(ale_fix)

    " Asynchronous completion
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    Plug 'fszymanski/deoplete-emoji'              " Auto-complete emojus
    Plug 'deoplete-plugins/deoplete-dictionary'   " Auto-complete dictionary word
    Plug 'thalesmello/webcomplete.vim'            " Auto-complete from open browser
      let g:deoplete#enable_at_startup = 1

      function s:DeopleteHooks()
        " These might fail if deoplete is not yet installed
        call deoplete#custom#option({
              \ 'auto_complete_delay': 500,
              \ 'smart_case': v:true,
              \ })

        call deoplete#custom#option('sources', {
              \ 'rust': ['ale', 'around', 'buffer'],
              \})

        call deoplete#custom#source('emoji', 'filetypes', [
              \ 'gitcommit',
              \ 'markdown',
              \ 'txt',
              \ 'rst',
              \ 'vimwiki',
              \])
      endfunction
      let g:plug_callbacks += [function('s:DeopleteHooks')]

    Plug 'SirVer/ultisnips'                     " Snippet management
      let g:UltiSnipsExpandTrigger='<C-s>'
      let g:UltiSnipsJumpForwardTrigger='<C-s>'
      let g:UltiSnipsJumpBackwardTrigger='<C-x>'
    Plug 'honza/vim-snippets'                   " Std lib for snippets
    Plug 'rbonvall/snipmate-snippets-bib'       " Snippets for .bib files

  endif " has('nvim')

endif " !s:env_embedded
" }}}

" Window/buffer management {{{
" ----------------------------------------------------------------------------
if !s:env_embedded
  Plug 'moll/vim-bbye'
  Plug 'zhaocai/GoldenView.Vim'       " Split buffer size management
    let g:goldenview__enable_default_mapping = 0
    " nmap <silent> <S-F8> <Plug>GoldenViewSwitchToggle
    command! GoldenViewToggle ToggleGoldenViewAutoResize
  Plug 'AndrewRadev/bufferize.vim'    " Command contents in buffer
  Plug 'AndrewRadev/linediff.vim'     " Vimdiff line ranges
endif

" }}}

" File system {{{
" ----------------------------------------------------------------------------
if !s:env_embedded
  Plug 'tpope/vim-eunuch'             " UNIX-like functionality in Vim
  Plug 'ojroques/vim-oscyank'         " Yank across the terminal
  Plug 'farmergreg/vim-lastplace'     " Open where last opened
  Plug 'duggiefresh/vim-easydir'      " Create directories when non-existent
  Plug 'strboul/urlview.vim'          " See all URLs in buffer
endif
" }}}

" Writing {{{
" ----------------------------------------------------------------------------
Plug 'gyim/vim-boxdraw'                 " Draw ASCII text boxes
Plug 'joom/latex-unicoder.vim'          " Useful for 'pretty' Coq/Lean files
  let g:unicoder_cancel_normal = 1
  let g:unicoder_cancel_insert = 1
  let g:unicoder_cancel_visual = 1
  inoremap <C-g>u <Esc>:call unicoder#start(1)<CR>
Plug 'tpope/vim-characterize'           " Use ga to see metadata about unicode
Plug 'godlygeek/tabular'                " Align text
" }}}

" Simple Utilities {{{
" ----------------------------------------------------------------------------
" Low-cost utilities that stay out of my way but are handy to keep around
" (i.e., stuff I think should have been built into Vim/available as settings).

" Normal mode {{{
Plug 'tpope/vim-repeat'                   " User-defined dot-repeatable actions
Plug 'tpope/vim-commentary'               " use gcc to comment things out
Plug 'tpope/vim-unimpaired'               " ]* and [* mappings
Plug 'tpope/vim-speeddating'              " increment/decrement dates
  let g:speeddating_no_mappings = 1       " <C-S> to increment
  " force a non-recursive map to the fallback functions
  nnoremap <Plug>SpeedDatingFallbackUp    <C-A>
  nnoremap <Plug>SpeedDatingFallbackDown  <C-X>
  xnoremap <Plug>SpeedDatingFallbackUp    <C-A>
  xnoremap <Plug>SpeedDatingFallbackDown  <C-X>
  nmap  <C-S>   <Plug>SpeedDatingUp
  nmap  <C-X>   <Plug>SpeedDatingDown
  xmap  <C-S>   <Plug>SpeedDatingUp
  xmap  <C-X>   <Plug>SpeedDatingDown

Plug 'svermeulen/vim-cutlass'               " x and D only delete, no yank/cut
                                            " but retain cut behavior for d
  nnoremap d  d
  xnoremap d  d
  vnoremap d  d
  nnoremap dd dd

Plug 'andymass/vim-matchup'               " %-navigate user-defined pairs

Plug 'tommcdo/vim-exchange'               " Exchange text with repeated cx{motion}
Plug 'AndrewRadev/sideways.vim'           " Move things sideways in lists
  nnoremap cl :SidewaysRight<cr>
  nnoremap ch :SidewaysLeft<cr>

" }}}

" Visual mode {{{
Plug 'vim-scripts/vis'                    " Use :B for block mode commands
Plug 'matze/vim-move'                     " Move blocks in visual mode
  vmap <C-j> <Plug>MoveBlockDown
  vmap <C-l> <Plug>MoveBlockRight
  vmap <C-h> <Plug>MoveBlockLeft
  vmap <C-k> <Plug>MoveBlockUp
Plug 'nixon/vim-vmath'                    " Basic stats on visual selection
  vmap <expr>  ++  VMATH_YankAndAnalyse()
  nmap         ++  vip++

" }}}

if s:env_embedded
  " These are deprecated in favor of vim-sandwich, which does both and more.
  " However, it's a pretty utility we don't want to use/watch break in
  " complicated environments, so in these cases we stick with the classics:
  Plug 'tpope/vim-surround'                 " ds, cs, ys to change text surroundings
  Plug 'AndrewRadev/dsf.vim'                " Delete/change surrounding function
endif

" Insert mode {{{
if !s:env_embedded
  Plug 'tpope/vim-endwise'                " Write endings
  Plug 'jiangmiao/auto-pairs'             " Auto-insert brackets/parens/quotes
  " Plug 'LunarWatcher/auto-pairs'          " Auto-insert brackets/parens/quotes
    let g:AutoPairsCenterLine = 0         " Preserve buffer view
    let g:AutoPairsShortcutToggle = ''    " No need to toggle in insert mode
    let g:AutoPairsMapCh = 0              " Use <C-h> to only backspace
    let g:AutoPairsMapSpace = 0           " Don't go too crazy xD
    let g:AutoPairsShortcutFastWrap = '<c-g>s'
    let g:AutoPairsShortcutJump = '<c-g>%'
    " Don't auto-pair ' --- used as apostrophe in prose and prime for OCaml
    " let g:AutoPairs = {'(':')', '[':']', '{':'}','"':'"', '`':'`'}
    command! AutoPair call AutoPairsToggle()
    augroup auto_filetypes
      autocmd!
      autocmd Filetype html let b:AutoPairs = AutoPairsDefine({'<!--' : '-->'}, ['{'])
      autocmd Filetype tex let b:AutoPairs = AutoPairsDefine({}, ["'", "`"])
    augroup END
endif

" }}}

" }}}

" Rich Utilities {{{
" ----------------------------------------------------------------------------
" Utilities that have a steeper learning curve than Simple Utilities,
" and are probably not as portable, but are not as involved as Interactive Subsystems.

if !s:env_embedded
  Plug 'gcmt/wildfire.vim'                    " Smart text object selection
    let g:wildfire_objects = {
        \ "*" : ["i'", 'i"', "i)", "i]", "i}"],
        \ "html,xml" : ["at", "it"],
        \ "vim" : ["i<"],
    \ }

  Plug 'AndrewRadev/splitjoin.vim'            " Toggle between single-/multi-line syntax
    let g:splitjoin_split_mapping = 'gK'
    let g:splitjoin_join_mapping = 'gJ'

  Plug 'justinmk/vim-sneak'                   " s works like f/t but with two chars
    let g:sneak#label = 1                     " Easy-motion-like labels
    let g:sneak#s_next = 1                    " Empty search uses most recent recent

    " 2-character Sneak (default)
    nmap gz <Plug>Sneak_s
    nmap gZ <Plug>Sneak_S
    xmap z <Plug>Sneak_s
    xmap Z <Plug>Sneak_S
    omap z <Plug>Sneak_s
    omap Z <Plug>Sneak_S

    " Already mapped ; to :
    map , <Plug>Sneak_;
    map ]f <Plug>Sneak_;
    map [f <Plug>Sneak_,

    " 1-character enhanced 'f'
    nmap f <Plug>Sneak_f
    nmap F <Plug>Sneak_F
    xmap f <Plug>Sneak_f
    xmap F <Plug>Sneak_F
    omap f <Plug>Sneak_f
    omap F <Plug>Sneak_F

    " 1-character enhanced 't'
    nmap t <Plug>Sneak_t
    nmap T <Plug>Sneak_T
    xmap t <Plug>Sneak_t
    xmap T <Plug>Sneak_T
    omap t <Plug>Sneak_t
    omap T <Plug>Sneak_T
    omap T <Plug>Sneak_T

  " Plug 'svermeulen/vim-yoink'       " Yoink (yank) ring
  "   " nmap p <plug>(YoinkPaste_p)
  "   " nmap P <plug>(YoinkPaste_P)

  "   " First time we hit p, paste; subsequent times cycles through yoink ring
  "   nmap <expr> p yoink#canSwap() ? '<plug>(YoinkPostPasteSwapBack)' : '<plug>(YoinkPaste_p)'
  "   nmap <expr> P yoink#canSwap() ? '<plug>(YoinkPostPasteSwapForward)' : '<plug>(YoinkPaste_P)'

  "   nmap [p <plug>(YoinkRotateBack)
  "   nmap ]p <plug>(YoinkRotateForward)
  "   nmap <c-=> <plug>(YoinkPostPasteToggleFormat)
  "   let g:yoinkSwapClampAtEnds = 0              " allow cycling through yank ring
  "   let g:yoinkIncludeDeleteOperations = 1
  "   let g:yoinkMoveCursorToEndOfPaste = 1
  "   let g:yoinkSyncSystemClipboardOnFocus = 0

  Plug 'svermeulen/vim-subversive'    " Substitute from yank
    let g:subversivePreserveCursorPosition = 1
    let g:subversivePromptWithActualCommand = 1
    nmap s <plug>(SubversiveSubstitute)
    nmap ss <plug>(SubversiveSubstituteLine)
    nmap S <plug>(SubversiveSubstituteToEndOfLine)
    " Substitute word under cursor in motion
    nmap <leader>ss <plug>(SubversiveSubstituteWordRange)
    nmap <leader>css <plug>(SubversiveSubstituteWordRangeConfirm)
    " Paste in visual mode
    xmap p <plug>(SubversiveSubstitute)
    xmap P <plug>(SubversiveSubstitute)

  Plug 'lfv89/vim-interestingwords'   " * but better and still lightweight
    let g:interestingWordsDefaultMappings = 0
    nnoremap <silent> gm :call InterestingWords('n')<cr>
    vnoremap <silent> gm :call InterestingWords('v')<cr>
    nnoremap <silent> gM :call UncolorAllWords()<cr>

    nnoremap <silent> m] :call WordNavigation(1)<cr>
    nnoremap <silent> m[ :call WordNavigation(0)<cr>

    command! Noh noh | call UncolorAllWords()

  " Fancier vim-surround + d
  Plug 'machakann/vim-sandwich'
    function s:SandwichCallback()
      " Use vim-surround bindings
      runtime macros/sandwich/keymap/surround.vim

      if !exists('g:sandwich#recipes')
        echom 'Sandwich does not appear to be installed, skipping hook.'
        return
      endif

      " Add spaces after
      let g:sandwich#recipes += [
            \   {'buns': ['{ ', ' }'], 'nesting': 1, 'match_syntax': 1, 'kind': ['add', 'replace'], 'action': ['add'], 'input': ['{']},
            \   {'buns': ['[ ', ' ]'], 'nesting': 1, 'match_syntax': 1, 'kind': ['add', 'replace'], 'action': ['add'], 'input': ['[']},
            \   {'buns': ['( ', ' )'], 'nesting': 1, 'match_syntax': 1, 'kind': ['add', 'replace'], 'action': ['add'], 'input': ['(']},
            \   {'buns': ['{\s*', '\s*}'],   'nesting': 1, 'regex': 1, 'match_syntax': 1, 'kind': ['delete', 'replace', 'textobj'], 'action': ['delete'], 'input': ['{']},
            \   {'buns': ['\[\s*', '\s*\]'], 'nesting': 1, 'regex': 1, 'match_syntax': 1, 'kind': ['delete', 'replace', 'textobj'], 'action': ['delete'], 'input': ['[']},
            \   {'buns': ['(\s*', '\s*)'],   'nesting': 1, 'regex': 1, 'match_syntax': 1, 'kind': ['delete', 'replace', 'textobj'], 'action': ['delete'], 'input': ['(']},
            \ ]

      " We also add text object targets for interacting with inner surroundings:
      xmap is <Plug>(textobj-sandwich-query-i)
      xmap as <Plug>(textobj-sandwich-query-a)
      omap is <Plug>(textobj-sandwich-query-i)
      omap as <Plug>(textobj-sandwich-query-a)
      xmap iss <Plug>(textobj-sandwich-auto-i)
      xmap ass <Plug>(textobj-sandwich-auto-a)
      omap iss <Plug>(textobj-sandwich-auto-i)
      omap ass <Plug>(textobj-sandwich-auto-a)
      xmap im <Plug>(textobj-sandwich-literal-query-i)
      xmap am <Plug>(textobj-sandwich-literal-query-a)
      omap im <Plug>(textobj-sandwich-literal-query-i)
      omap am <Plug>(textobj-sandwich-literal-query-a)

      augroup SandwichFiletypes
        autocmd!
        autocmd FileType python call sandwich#util#addlocal([
          \   {'buns': ['"""', '"""'], 'nesting': 0, 'input': ['3"']},
          \ ])

      augroup END
    endfunction
    let g:plug_callbacks += [function('s:SandwichCallback')]

endif " !s:env_embedded

" }}}

" File types {{{
" ----------------------------------------------------------------------------
if !s:env_embedded

" TeX/LaTeX {{{
  Plug 'lervag/vimtex',   { 'for': 'tex' }    " TeX/LaTeX
    let g:tex_flavor = 'latex'
    let g:vimtex_compiler_latexmk = {
      \ 'continuous' : 0,
      \}

    " Automatically open quickfix, but do not focus
    let g:vimtex_quickfix_mode = 2
    let g:vimtex_quickfix_autoclose_after_keystrokes = 4
    let g:vimtex_quickfix_ignore_filters = [
      \ 'Font shape declaration has incorrect series value',
      \ 'You are using breakurl while processing',
      \ 'Underfull',
      \ 'Overfull',
      \]

    let g:tex_conceal = 'abdmg'
    let g:vimtex_mappings_enabled = 0
    let g:vimtex_imaps_enabled = 0
    let g:vimtex_view_method = 'zathura'
    let g:vimtex_complete_enabled = 1
    " let g:vimtex_disable_recursive_main_file_detection = 1

    function! VimtexConfig()
      imap <buffer> <C-]>             <plug>(vimtex-delim-close)

      nmap <buffer> <C-c><CR>         <plug>(vimtex-compile-ss)
      vmap <buffer> <C-c><CR>    <ESC><plug>(vimtex-compile-ss)
      imap <buffer> <C-c><CR>    <ESC><plug>(vimtex-compile-ss)
      nmap <buffer> <C-c>l            <plug>(vimtex-compile-ss)
      vmap <buffer> <C-c>l       <ESC><plug>(vimtex-compile-ss)
      imap <buffer> <C-c>l       <ESC><plug>(vimtex-compile-ss)

      nmap <buffer> <C-c><Space>      <plug>(vimtex-view)
      vmap <buffer> <C-c><Space> <ESC><plug>(vimtex-view)
      imap <buffer> <C-c><Space> <ESC><plug>(vimtex-view)

      nmap <buffer> <C-c>c            <plug>(vimtex-errors)
      vmap <buffer> <C-c>c       <ESC><plug>(vimtex-errors)
      imap <buffer> <C-c>c       <ESC><plug>(vimtex-errors)

      imap <buffer> <C-g>e      \emph{}<left>
      imap <buffer> <C-g>t      \texttt{}<left>
      imap <buffer> <C-g>b      \textbf{}<left>
      imap <buffer> <C-g>i      \textit{}<left>

      " This might fail if deoplete or vimtex are not installed
      call deoplete#custom#var('omni', 'input_patterns', {
            \ 'tex': g:vimtex#re#deoplete,
            \})
    endfunction

    augroup vimtex_settings
      autocmd!
      autocmd Filetype tex call VimtexConfig()
    augroup END
" }}}

" Coq {{{
  Plug 'whonore/coqtail', { 'for': 'coq' }
    augroup CoqtailHighlights
      autocmd!
      autocmd ColorScheme *
            \   highlight def CoqtailChecked ctermbg=236
            \|  highlight def CoqtailSent  ctermbg=237
    augroup END
    let g:coqtail_match_shift = 1
    let g:coqtail_indent_on_dot = 1
    let g:coqtail_auto_set_proof_diffs = "on"

    let g:coqtail_update_tagstack = 1

    " let g:coqtail_map_prefix = '<C-c>'
    let g:coqtail_nomap = 1

    function DefineCoqtailMappings()
        " Coqtail control
        nmap <buffer> <c-c>Q        <Plug>CoqStart
        nmap <buffer> <c-c>q        <Plug>CoqStop
        nmap <buffer> <c-c>D        <Plug>CoqToggleDebug
        nmap <buffer> <c-c>r        <Plug>CoqRestorePanels
        nmap <buffer> <c-c><c-c>    <Plug>CoqRestorePanels

        " Checked region navigation
        nmap <buffer> <c-c>.                <Plug>CoqToLine
        imap <buffer> <c-c>.           <Esc><Plug>CoqToLine
        nmap <buffer> <c-c>l             mz$<Plug>CoqToLine`z
        imap <buffer> <c-c>l        <Esc>mz$<Plug>CoqToLine`z
        nmap <buffer> <c-c><CR>          mz$<Plug>CoqToLine`z
        imap <buffer> <c-c><CR>     <Esc>mz$<Plug>CoqToLine`z
        nmap <buffer> <c-c>j                <Plug>CoqNext
        nmap <buffer> <c-c>k                <Plug>CoqUndo
        nmap <buffer> <c-c>h                <Plug>CoqJumpToEnd

        " Goal buffer navigation
        nmap <buffer> <c-c>g        <Plug>CoqGotoGoalStart
        nmap <buffer> <c-c>G        <Plug>CoqGotoGoalEnd
        nmap <buffer> <c-c>]]       <Plug>CoqGotoGoalNextStart
        nmap <buffer> <c-c>][       <Plug>CoqGotoGoalNextEnd
        nmap <buffer> <c-c>[[       <Plug>CoqGotoGoalPrevStart
        nmap <buffer> <c-c>[]       <Plug>CoqGotoGoalPrevEnd

        " Semantic features
        nmap <buffer> <c-c><c-]>    <Plug>CoqGotoDef
        nmap <buffer> <c-c>c        <Plug>CoqCheck
        xmap <buffer> <c-c>c        <Plug>CoqCheck
        nmap <buffer> <c-c>a        <Plug>CoqAbout
        xmap <buffer> <c-c>a        <Plug>CoqAbout
        nmap <buffer> <c-c>s        <Plug>CoqSearch
        xmap <buffer> <c-c>s        <Plug>CoqSearch
        nmap <buffer> <c-c>d        <Plug>CoqPrint
        xmap <buffer> <c-c>d        <Plug>CoqPrint
        nmap <buffer> <c-c>f        <Plug>CoqLocate
        xmap <buffer> <c-c>f        <Plug>CoqLocate
    endfunction

    augroup coqtail_mappings
      autocmd!
      autocmd Filetype coq,coq-infos,coq-goals call DefineCoqtailMappings()
      autocmd Filetype coq syntax sync fromstart
    augroup END
" }}}

" Markdown {{{
  Plug 'plasticboy/vim-markdown',           { 'for': 'markdown' }
    let g:vim_markdown_math = 1
    let g:vim_markdown_auto_insert_bullets = 0
    let g:vim_markdown_folding_style_pythonic = 1
    function MarkdownHook()
      nmap g] <Plug>Markdown_MoveToCurHeader
      nmap g[ <Plug>Markdown_MoveToParentHeader
    endfunction

    augroup markdown_mappings
      autocmd!
      autocmd Filetype markdown call MarkdownHook()
    augroup END
  " Plug 'tpope/vim-markdown',                { 'for': 'markdown' }
    " let g:markdown_fenced_languages = [
    "       \ 'html',
    "       \ 'python',
    "       \ 'bash=sh',
    "       \ 'c',
    "       \ 'cpp',
    "       \ 'ocaml',
    "       \ 'haskell'
    "       \ ]
  " Plug 'jtratner/vim-flavored-markdown',    { 'for': 'markdown' }
" }}}

" Other filetypes {{{
  Plug 'z0mbix/vim-shfmt',                  { 'for': 'sh' }
    let g:shfmt_extra_args = '-i 2 -ci -sr'
  Plug 'fatih/vim-go',                      { 'for': 'go' }
  Plug 'leanprover/lean.vim',               { 'for': 'lean' }
  Plug 'idris-hackers/idris-vim',           { 'for': 'idris' }
  Plug 'LnL7/vim-nix',                      { 'for': 'nix' }
  Plug 'vim-scripts/promela.vim',           { 'for': 'promela' }
  Plug 'chrisbra/csv.vim',                  { 'for': 'csv' }
  Plug 'rust-lang/rust.vim',                { 'for': 'rust' }
    " let g:rust_conceal = 1
    let g:rust_recommended_style = 0
    " let g:rust_conceal_mod_path = 1
    " let g:rust_conceal_pub = 1
  Plug 'LucHermitte/valgrind.vim',          { 'for': 'c' }
    let g:valgrind_arguments='--leak-check=yes '
  Plug 'dag/vim-fish',                      { 'for': 'fish' }
  Plug 'adborden/vim-notmuch-address',      { 'for': 'mail' }
" }}}
endif " !s:env_embedded
" }}}


" Local settings
" ----------------------------------------------------------------------------
"  Note: This has to be here, in the Plugins block, because otherwise I can't
"    use Plug
if filereadable(expand("~/.vim_local"))
  source ~/.vim_local
endif

call plug#end()

" try
  for Cb in g:plug_callbacks
    call Cb()
  endfor
" catch
"     echom 'Encountered errors when executing plug callbacks, run :PlugInstall'
" endtry

" }}}

" ============================================================================
" Settings {{{
" ============================================================================

" Appearance {{{
" ----------------------------------------------------------------------------

set background=dark
set nu rnu                " Line numbers and relative line numbers
set display+=lastline     " Show as much as possible of the last line
set scrolloff=5           " Keep a few lines under the cursor
set sidescrolloff=2       " Keep a few lines to the side of the cursor
set cmdheight=2           " Extra space for ALE hover output
set statusline=2          " TODO: Uhhh no idea what this does

augroup cursor_underline  " Underline cursor in insert mode
  autocmd!
  autocmd InsertEnter * set cul
  autocmd InsertLeave * set nocul
augroup END

" set nowrap                            " Don't show wrapped lines
set linebreak                           " If we show wrap, break at a character
set breakindent                         " ... and try to make it look nice
set breakindentopt=sbr,min:48,shift:8   " ... with these options
let &showbreak='  ⇒ '                   " ... and this nice symbol.

set colorcolumn=80,120,121,+1,+2        " Columns at 80, 120, and textwidth

set list                                " That mysterious, poorly named option
                                        " that changes how things are displayed,
                                        " as configured by the following option:
set listchars=tab:\┆\ ,trail:·,extends:‥,precedes:‥

set conceallevel=2                      " Concealed text is completely hidden unless
                                        " custom replacement character is defined
set concealcursor=                      " Show concealed text when running cursor over

set foldlevelstart=10
set foldnestmax=10
set foldmethod=manual

" Darken Pmenu background so that it doesn't merge with Cursorline or ColorColumn
highlight Pmenu ctermbg=233

augroup ron_color_tweaks " {{{
  autocmd!
  " autocmd ColorScheme ron
  "   \   highlight clear Conceal
  "   \|  highlight clear VertSplit
  "   \|  highlight SignColumn  ctermbg=NONE cterm=NONE guibg=NONE gui=NONE
  "   \|  highlight ColorColumn   ctermbg=234 guibg=#1c1c1c
  "   \|  highlight Folded    ctermbg=234 guibg=#1c1c1c
  "   \|  highlight FoldColumn  ctermbg=234 guibg=#1c1c1c
  " autocmd ColorScheme ron
  "   \   highlight Search  cterm=underline,bold ctermfg=blue ctermbg=234
  "   \             gui=underline,bold   guifg=blue guibg=#1c1c1c
  "   \|  highlight IncSearch cterm=underline,bold ctermfg=cyan ctermbg=239
  "   \             gui=underline,bold   guifg=cyan guibg=#4e4e4e
  " autocmd ColorScheme ron
  "   \   highlight Pmenu     ctermbg=234 ctermfg=white
  "   \               guibg=#1c1c1c guifg=white
  "   \|  highlight PmenuSbar   ctermbg=240 ctermfg=white
  "   \               guibg=#585858 guifg=white
  "   \|  highlight PmenuThumb  ctermbg=240 ctermfg=white
  "   \               guibg=#585858 guifg=white
  "   \|  highlight PmenuSel    ctermbg=240 ctermfg=white cterm=bold
  "   \               guibg=#585858 guifg=white gui=bold
  "   \|  highlight TabLineSel  ctermbg=240 ctermfg=white
  "   \               guibg=#1c1c1c guifg=white
  "   \|  highlight TabLine     ctermbg=234 ctermfg=240
  "   \               guibg=#1c1c1c guifg=#585858
  "   \|  highlight TabLineFill   ctermfg=234
  "   \               guibg=#1c1c1c
  " autocmd ColorScheme ron
  "   \   highlight htmlItalic        term=standout
  "   \                     ctermfg=121
  "   \                     guifg=Green
  "   \|  highlight htmlBoldItalic      term=bold,standout
  "   \                     cterm=bold ctermfg=121
  "   \                     gui=bold guifg=Green
  "   \|  highlight htmlUnderlineItalic     term=underline,standout
  "   \                     cterm=underline ctermfg=121
  "   \                     gui=underline guifg=Green
  "   \|  highlight htmlBoldUnderlineItalic   term=underline,bold,standout
  "   \                     cterm=underline,bold ctermfg=121
  "   \                     gui=underline,bold guifg=Green
  " autocmd ColorScheme ron
  "   \   highlight SpellBad    ctermbg=NONE ctermfg=red
  "   \               guibg=NONE   guifg=red    gui=undercurl
  "   \|  highlight SpellRare   ctermbg=NONE ctermfg=yellow
  "   \               guibg=NONE   guifg=yellow   gui=undercurl
  "   \|  highlight SpellCap    ctermbg=NONE ctermfg=cyan
  "   \               guibg=NONE   guifg=cyan   gui=undercurl
  "   \|  highlight SpellLocal  ctermbg=NONE ctermfg=yellow
  "   \               guibg=NONE   guifg=yellow   gui=undercurl
  " autocmd ColorScheme
  "   \   highlight DiffAdd     ctermbg=17    cterm=bold
  "   \               guibg=#00005f   gui=bold
  "   \|  highlight DiffDelete  ctermbg=234   ctermfg=242
  "   \               guibg=#1c1c1c   guifg=#6c6c6c
  "   \|  highlight DiffChange  ctermbg=234
  "   \               guibg=#1c1c1c
  "   \|  highlight DiffText    ctermbg=234   cterm=underline
  "   \               guibg=#1c1c1c   gui=undercurl
  "   autocmd ColorScheme ron highlight MatchParen guifg=red
augroup END " }}}

" }}}

" Navigation {{{
" ----------------------------------------------------------------------------

set hidden                      " Jump away files even when unsaved

set backspace=indent,eol,start  " Backspacing over everything in insert mode
set nostartofline               " Prevent cursor from jumping to start of line

set showmatch                   " Show matching brackets
set virtualedit=block,onemore   " Move cursor end of line

set splitright                  " Direction of split

set hlsearch                    " Highlight search
set incsearch                   " Incremental search
set ignorecase                  " Ignores case
set smartcase                   " Smart case
set wrapscan                    " Jump back to top
if has('nvim')
  set inccommand=split          " Preview substitution in split window
endif

set wildmenu                    " Use wildmenu
set wildmode=longest:full,full  " Sane completion interface
set wildignorecase              " Ignore case during completion

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

set textwidth=80    " Bound lines to 80 characters
set expandtab       " Expand tabs to spaces
set tabstop=4       " Expand tabs to 4 spaces
set shiftwidth=0    " Use tabstop value for (auto)indent
set smarttab      " Apply tabs in front of a line according to shiftwidth
set autoindent      " Automatically indent when starting a new line
set nojoinspaces    " Only insert single space after J
set formatoptions+=j  " Strip comment leader when joining comment lines
set formatoptions+=n  " Recognize numbered lists when formatting text
set formatoptions+=l  " Don't break up my text when in insert mode
set formatoptions+=1  " Don't break up a line after a one-letter word
set formatoptions-=t  " Don't auto-wrap text (code)
set formatoptions-=c  " Don't auto-wrap comments either

" }}}

" }}}

" ============================================================================
" Key bindings {{{
" ============================================================================

" NOTE: some readline-style bindings cherrypicked/simplified from tpope/vim-rsi,

if &encoding ==# 'latin1' && has('gui_running') && !empty(findfile('plugin/sensible.vim', escape(&rtp, ' ')))
  set encoding=utf-8
endif

function! s:MapMeta() abort
  noremap!    <M-b> <S-Left>
  noremap!    <M-f> <S-Right>
  noremap!    <M-d> <C-O>dw
  cnoremap    <M-d> <S-Right><C-W>
  noremap!    <M-n> <Down>
  noremap!    <M-p> <Up>
  noremap!    <M-BS> <C-W>
  noremap!    <M-C-h> <C-W>
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
  noremap!    <F29> <S-Left>
  noremap!    <F30> <S-Right>
  noremap!    <F31> <C-O>dw
  cnoremap    <F31> <S-Right><C-W>
  noremap!    <F32> <Down>
  noremap!    <F33> <Up>
  noremap!    <F34> <C-W>
  noremap!    <F35> <C-W>
  augroup rsi_gui
  autocmd GUIEnter * call s:MapMeta()
  augroup END
endif

" Appearance {{{
" ----------------------------------------------------------------------------

" Wrangle folds from jumping
nnoremap za zazz
nnoremap zi zizz

nnoremap Q zazz
" }}}


" Navigation {{{
" ----------------------------------------------------------------------------

" Block-oriented (non-linewise) navigation
nnoremap j gj
nnoremap k gk

" Get out insert mode easily
inoremap <C-c>  <Esc>
" inoremap kj   <Esc>

" Normal mode readline style navigation
nnoremap <C-n>    <C-e>j
nnoremap <C-p>    <C-y>k
nnoremap <C-e>    $
nnoremap <C-a>    ^
nnoremap <C-f>    l
nnoremap <C-b>    h

" Virual mode readline style navigation
vnoremap <C-n>    <C-e>j
vnoremap <C-p>    <C-y>k
vnoremap <C-e>    $
vnoremap <C-a>    ^
vnoremap <C-f>    l
vnoremap <C-b>    h

" Window navigation
noremap <C-w>n <Esc>:bn<CR>
noremap <C-w>p <Esc>:bp<CR>
noremap <C-w>q <Esc>:bd<CR>

inoremap <C-F> <Right>
inoremap <C-B> <Left>
" inoremap   <C-X><C-A> <C-A>
inoremap <C-A> <C-O>^

inoremap    <C-n> <down>
inoremap    <C-p> <up>

function! s:i_ctrl_e()
  " If in context menu, accept selection
  if pumvisible()
    return "\<C-y>"
  endif
  if col('.') > strlen(getline('.'))
    return "\<C-e>" " if at line end, fallback to default <C-e> behavior
  endif
  " Otherwise, go to <End> of line
  return "\<End>"
endfunction

inoremap <expr> <C-e> <SID>i_ctrl_e()

" If we are already at the end of the line, fall back to default <C-e> behavior (insert character from next line)
" inoremap <expr> <C-e> col('.')>strlen(getline('.'))<bar><bar>pumvisible()?"\<Lt>C-y>":"\<Lt>End>"

" I don't really use default bindings for <C-f> or <C-b>
" inoremap <expr> <C-F> col('.')>strlen(getline('.'))?"\<Lt>C-F>":"\<Lt>Right>"
" inoremap <expr> <C-B> getline('.')=~'^\s*$'&&col('.')>strlen(getline('.'))?"0\<Lt>C-D>\<Lt>Esc>kJs":"\<Lt>Left>"

" }}}

" Editing {{{
" ----------------------------------------------------------------------------

" Don't leave visual mode when indending
xnoremap < <gv
xnoremap > >gv

" " No clue what the hell this was supposed to do
" inoremap # X#

" Correct spelling
inoremap <C-l> <c-g>u<Esc>[s1z=`]a<c-g>u

" Insert date/time
nnoremap <space>id :put =strftime(\"%Y-%m-%d\")<CR>
inoremap <C-g>d <C-R>=strftime("%Y-%m-%d")<CR>
nnoremap <space>it :put =strftime(\"%Y-%m-%d %H:%M:%S\")<CR>
inoremap <C-g>t <C-R>=strftime("%Y-%m-%d %H:%M:%S")<CR>

" Auto indentation
inoremap <C-g><C-g> <C-F>

" For readline-style line deletion, we use the delete register "-
nnoremap <C-k> "-D
inoremap <C-k> <C-o>"-D
nnoremap <C-y> "-p
inoremap <C-y> <C-o>"-p

" inoremap <expr> <C-D> col('.')>strlen(getline('.'))?"\<Lt>C-D>":"\<Lt>Del>"

" }}}

" Command mode {{{
" ----------------------------------------------------------------------------

" Eliminate extra key press
nnoremap ; :
vnoremap ; :

" Don't use <Left> and <Right> key for selecting previous/next match
cnoremap <Left> <Space><BS><Left>
cnoremap <Right> <Space><BS><Right>

cnoremap <C-F> <Right>
cnoremap <C-B> <Left>

" cnoremap   <C-X><C-A> <C-A>
cnoremap   <C-A> <Home>
cnoremap <expr> <C-E> col('.')>strlen(getline('.'))?"\<Lt>C-E>":"\<Lt>End>"
" Note that <C-e> also "steps into" wild menu

" cnoremap <expr> <C-D> getcmdpos()>strlen(getcmdline())?"\<Lt>C-D>":"\<Lt>Del>"
" cnoremap <expr> <C-F> getcmdpos()>strlen(getcmdline())?&cedit:"\<Lt>Right>"

function! s:c_ctrl_u()
  " If in context menu, simulate page up-ish behavior
  if pumvisible()
    return repeat("\<c-p>", 10)
  endif

  " Otherwise, delete to beginning of line, stash contents in small delete register
  if getcmdpos() > 1
    let @- = getcmdline()[:getcmdpos()-2]
  endif
  return "\<C-U>"
endfunction

function! s:c_ctrl_d()
  " If in context menu, simulate page down-ish behavior
  if pumvisible()
    return repeat("\<c-n>", 10)
  endif
  " Otherwise, simulate delete
  return "\<Del>"
endfunction

function! s:c_ctrl_k()
  " Simulate kill line, stash contents in small delete register
  if getcmdpos() > 0
    let @- = getcmdline()[getcmdpos()-1:]
  endif
  return "\<C-\>e(strpart(getcmdline(), 0, getcmdpos() - 1))\<CR>"
endfunction

cnoremap <expr> <C-u> <SID>c_ctrl_u()
cnoremap <expr> <C-d> <SID>c_ctrl_d()
cnoremap <expr> <C-k> <SID>c_ctrl_k()

" Yank from delete register
cnoremap <C-Y> <C-R>-

" }}}
" }}}

" ============================================================================
" Commands {{{
" ============================================================================

" :Qa (quality assurance for my typos) {{{
command! Qa qa
" }}}

" Over80/120: highlight characters past 80/120 {{{
command! Over80 normal! m`/\%>80v./+<CR>``
command! Over120 normal! m`/\%>120v./+<CR>``
" }}}

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

" Source .vimrc {{{
command! Vimrc source ~/.vimrc
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
command! Basic call s:basicToggle()
" }}}

let s:sharemode = 0
function! s:shareToggle()
  if s:sharemode
    set rnu nocursorline
    let s:sharemode = 0
  else
    set nornu cursorline
    let s:sharemode = 1
  endif
endfunction
command! Share call s:shareToggle()

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

" DOI: resolve a DOI {{{
" Example usage: :DOIR 10.1016/j.algal.2015.04.001
command! -nargs=1 DOIR r! curl -sLH "Accept: application/x-bibtex" https://dx.doi.org/<args>
command! -nargs=1 DOI r! curl -sLH "Accept: application/x-bibtex" <args>
" }}}

" BibCommas: add missing commas to BibTeX file {{{
command! -buffer -range=% -bar BibCommas keeppatterns
  \ <line1>,<line2>substitute:\v([}"])(\s*\n)+(\s*\a+\s*\=):\1,\2\3:giep
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
  autocmd!
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
        \ spell
augroup END " }}}

augroup bib_settings " {{{
  autocmd!
  autocmd Filetype bib setlocal
        \ tabstop=2
        \ expandtab
        \ shiftwidth=2
        \ softtabstop=2
        \ spell
augroup END " }}}

augroup markdown_settings " {{{
  autocmd!
  autocmd Filetype markdown setlocal
        \ tabstop=4
        \ expandtab
        \ shiftwidth=4
        \ softtabstop=4
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
augroup END " }}}

augroup csv_settings " {{{
  autocmd!
  autocmd BufNewFile,BufReadPost *.csv set filetype=csv
augroup END " }}}

" }}}

" vim: set ts=2 sw=2 tw=120 et foldmethod=marker:

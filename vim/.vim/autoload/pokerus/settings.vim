scriptencoding utf-8

function s:BackupFiles()
  silent! !mkdir -p ~/.tmp/{backup,swp,undo}.{nvim,vim}

  if has('nvim')
    set backupdir=~/.tmp/backup.nvim//,.
    set directory=~/.tmp/swp.nvim//,.
    set undodir=~/.tmp/undo.nvim//,.
  else
    set backupdir=~/.tmp/backup.vim//,.
    set directory=~/.tmp/swp.vim//,.
    set undodir=~/.tmp/undo.vim//,.
  endif

  set backup
  set swapfile
  set undofile

  set undolevels=1000
  if has('persistent_undo')
    set undofile
    set undoreload=10000
  endif
endfunction

function s:TerminalIO()
  set mouse=a       " Mouse interaction

  for i in range(2, 12) " F2...F12 seem to cause Neovim to panic
    exe 'inoremap <F' . string(i) . '> F' . string(i)
  endfor

  set noerrorbells visualbell t_vb=     " No error bell

  " If terminal supports displaying italics, we need these key sequences
  let t_ZH="\e[3m"
  let t_ZR="\e[23m"
  set updatetime=100

  " Print undercurl
  let &t_Cs = "\e[4:3m"
  let &t_Ce = "\e[4:0m"

  " Disable background color erase
  let &t_ut=''

  " Helps Vim understand alacritty colors; nop for Neovim
  " From: https://www.reddit.com/r/vim/comments/kennw5/comment/gg4hwop/?utm_source=share&utm_medium=web2x&context=3
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

  if has('termguicolors')
    set termguicolors
  endif

  set lazyredraw
endfunction

function s:Clipboard()
  let os = substitute(system('uname'), '\n', '', '')
  if os ==? 'Darwin'
    set clipboard=unnamed
  elseif os ==? 'Linux'
    set clipboard=unnamedplus
  endif

  if $DISPLAY !=# ''
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
endfunction

function s:Appearance()
  set background=dark
  set number                " Line numbers
  set relativenumber        " Relative line numbers
  set display+=lastline     " Show as much as possible of the last line
  set scrolloff=5           " Keep a few lines under the cursor
  set sidescrolloff=2       " Keep a few lines to the side of the cursor
  set cmdheight=2           " Extra space in command line at bottom
  set noshowmode            " We already have lightline

  if has('nvim')
    " Neovim plugins I use tend to make a lot of use of the sign column
    set signcolumn=yes      " Sign column always visible
  else
    set signcolumn=number   " Sign column shares space with line number column
  endif

  augroup cursor_underline  " Underline cursor in insert mode
    autocmd!
    autocmd InsertEnter * set cul
    autocmd InsertLeave * set nocul
  augroup END

  " set nowrap                            " Soft wrap
  set linebreak                           " If we show wrap, break at a character
  set breakindent                         " ... and try to make it look nice
  set breakindentopt=sbr,min:48           " ... with these options
  let &showbreak=' ↪'                     " ... and this nice symbol
  set cpoptions+=n                        " ... shown in the line number column

  set colorcolumn=81,121,+1,+2           " Columns at 80, 120, and textwidth

  set list                                " That mysterious, poorly named option
                                          " that changes how things are displayed,
                                          " as configured by the following option:
  set listchars=tab:\┆\ ,trail:·,extends:‥,precedes:‥

  set conceallevel=2                      " Concealed text is completely hidden unless
                                          " custom replacement character is defined
  set concealcursor=                      " Show concealed text when running cursor over

  set foldlevelstart=10
  set foldnestmax=10
  set foldmethod=syntax

  set shortmess+=c                        " Don't show information about pop-up menu
endfunction

function s:Navigation()
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
  let g:netrw_bufsettings = 'noma nomod nonu nowrap ro buflisted'
endfunction

function s:Editing()
  set textwidth=80      " How wide text should be
  set expandtab         " Expand tabs to spaces
  set tabstop=4         " Expand tabs to 4 spaces
  set shiftwidth=0      " Use tabstop value for (auto)indent
  set smarttab          " Apply tabs in front of a line according to shiftwidth
  set autoindent        " Automatically indent when starting a new line
  set nojoinspaces      " Only insert single space after J

  set formatoptions+=q  " Allow formatting with gq
  set formatoptions+=r  " Automatically insert comment leader on <CR>
  set formatoptions+=j  " Strip comment leader when joining comment lines
  set formatoptions+=n  " Recognize numbered lists when formatting text
  set formatoptions+=l  " Don't break up text that is already beyond textwidth
  set formatoptions+=1  " Don't break up a line after a one-letter word
  set formatoptions-=t  " Don't auto-wrap code text
  set formatoptions-=c  " Don't auto-wrap comments either
  set formatoptions-=o  " Don't insert comment leader automatically

  " NOTE: plugins seem to be pretty inconsistent about respecting formatoptions.
  " Add to filetype-specific hooks below if misbehaving.
endfunction

function s:Spelling()
  setlocal spelllang=en_us
  set spellfile=~/.vim/spell/en.utf-8.add
endfunction

function s:Misc()
  set shell=bash
  set modeline
  set modelines=5
  set timeoutlen=300
  set ttimeoutlen=10

  let c_syntax_for_h = 1 " Instead of C++

  " let g:load_doxygen_syntax = 1 " Highlight Doxygen comments
  " let doxygen_javadoc_autobrief = 0
  " let g:doxygen_enhanced_color = 1
  highlight! link doxygenComment        cComment
  highlight! link doxygenBrief          cComment
  highlight! link doxygenBody           cComment
  highlight! link doxygenSpecial        cDefine
  highlight! link doxygenPrev           cDefine
  highlight! link doxygenHtmlCh         cDefine
  highlight! link doxygenHtmlCmd        cDefine
  highlight! link doxygenGroupDefine    cDefine
  highlight! link doxygenParam          cFunction
  highlight! link doxygenOther          cFunction
  highlight! link doxygenParamName      cType
  highlight! link doxygenHashLink       cInclude
  highlight! link doxygenEmphasisedWord htmlItalic
  highlight! link doxygenBoldWord       htmlBold
  highlight! link doxygenBriefLine      CLEAR
  highlight! link doxygenSpecialOnelineDesc   cString
  highlight! link doxygenSpecialMultilineDesc cComment

  let g:no_gitrebase_maps = 1
endfunction

function pokerus#settings#setup()
  call s:BackupFiles()
  call s:TerminalIO()
  call s:Clipboard()
  call s:Appearance()
  call s:Navigation()
  call s:Editing()
  call s:Spelling()
  call s:Misc()
endfunction

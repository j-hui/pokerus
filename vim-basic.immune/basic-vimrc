" An example vimrc file, adapted from /usr/share/vim/vim82/vimrc_example.vim.
" Please adapt further for your personal use.
"
" When in doubt, :h is your friend.  For example, to look up what the shiftwidth
" setting does, run:
"
"   :h 'shiftwidth'
"
" Written by John Hui, with inspiration from Bram Moolenaar, Jae Woo Lee, and
" Timothy Pope.

" When started as "evim", evim.vim will already have done these settings, bail
" out.
if v:progname =~? "evim"
  finish
endif

" Get the defaults that most users want.
source $VIMRUNTIME/defaults.vim

"""""""" Appearance

" enable basic features, voiding compatability with arcane Vim distributions
syntax enable
filetype plugin indent on

set encoding=utf-8  " insist on utf-8 encoding
set number          " show line numbers
set laststatus=2    " always show status line
set ruler           " show line and column numbers
set wildmenu        " allow command-line completion

set scrolloff=5     " leave 5 lines of breathing room at the top and botom
set sidescrolloff=5 " leave 5 characters of breathing room at the side

" display literal characters for tabs, trailing spaces, last visible column,
" first visible column, non-breakable spaces
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
" to see these characters, run :set list


""""""""" Backup and undo

set backup          " keep a backup file (restore to previous version)
set undofile        " keep an undo file (undo changes after closing)

" Run mkdir to make sure ~/.backup exists first
silent! !mkdir -p ~/.backup

" Prepend ~/.backup to backupdir so that Vim will look for that directory
" before littering the current dir with backups.
set backupdir^=~/.backup

" Also use ~/.backup for undo files.
set undodir^=~/.backup

" Also use ~/.backup for swap files. The trailing // tells Vim to incorporate
" full path into swap file names.
set dir^=~/.backup//


""""""""" Search

set hlsearch        " highlight all terms matching / and ? search
set incsearch       " start searching and highlighting as you type
set smartcase       " case-insensitive search by default, but opt into
                    " case-sensitive search when you include capital characters

" Use <C-L> to clear the highlighting of :set hlsearch.
if maparg('<C-L>', 'n') ==# ''
  nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
endif


"""""""" Indentation and spelling

set textwidth=80    " start wrapping lines longer than 80 characters
set shiftwidth=4    " auto-indent by 4 spaces, just as K&R decreed
set expandtab       " expand tabs to spaces...
set tabstop=8       " ...8 spaces to be precise

" To forcibly insert a tab character, type <C-V><Tab> (ctrl-v followed by TAB)

" Per-filetype indentation settings.
" We put these in an autocmd group, so that we can delete them easily.
augroup FiletypeIndentation
  autocmd!

  " Don't expand tabs in Makefiles
  autocmd FileType make setlocal noexpandtab

  " Don't do spell-checking on Vim help files
  autocmd FileType help setlocal nospell

  " My settings when editing *.txt files
  "   - automatically indent lines according to previous lines
  "   - replace tab with 8 spaces
  "   - when I hit tab key, move 2 spaces instead of 8
  "   - wrap text if I go longer than 76 columns
  "   - check spelling
  autocmd FileType text setlocal autoindent expandtab softtabstop=2 textwidth=76 spell spelllang=en_us
augroup END


"""""""" Misc.

" The matchit plugin makes the % command work better, and comes bundled with
" most Vim distributions.
packadd! matchit

set ttimeout        " for escaped terminal codes, only wait for...
set ttimeoutlen=100 " ...100ms

set autoread        " automatically detect external changes to open file

function s:PlugCoq()
  Plug 'whonore/coqtail', { 'for': 'coq' }
    augroup CoqtailHighlights
      autocmd!
      autocmd ColorScheme *
            \   highlight def CoqtailChecked ctermbg=236
            \|  highlight def CoqtailSent    ctermbg=237
    augroup END
    let g:coqtail_match_shift = 1
    let g:coqtail_indent_on_dot = 1
    let g:coqtail_auto_set_proof_diffs = 'on'

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
endfunction

function s:PlugLatex()
  Plug 'lervag/vimtex',   { 'for': 'tex' }
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
      imap <buffer> <C-g>]            <plug>(vimtex-delim-close)

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
    endfunction

    augroup vimtex_settings
      autocmd!
      autocmd Filetype tex call VimtexConfig()
    augroup END
endfunction

function s:PlugMarkdown()
  Plug 'tpope/vim-markdown',                { 'as': 'tpope-vim-markdown' }
    let g:markdown_fenced_languages = [
          \ 'html',
          \ 'python',
          \ 'bash=sh',
          \ 'c',
          \ 'cpp',
          \ 'ocaml',
          \ 'haskell',
          \ 'rust',
          \ ]
    let g:markdown_folding = 1
    let g:markdown_syntax_conceal = 1

  " Plug 'plasticboy/vim-markdown', { 'as': 'plasticboy-vim-markdown' }
    let g:vim_markdown_fenced_languages = g:markdown_fenced_languages
  "   let g:vim_markdown_auto_insert_bullets = 0
  "   let g:vim_markdown_folding_style_pythonic = 1
  "   " let g:vim_markdown_math = 1
  "   function MarkdownHook()
  "     nmap g] <Plug>Markdown_MoveToCurHeader
  "     nmap g[ <Plug>Markdown_MoveToParentHeader
  "   endfunction

  "   augroup markdown_mappings
  "     autocmd!
  "     autocmd Filetype markdown call MarkdownHook()
  "   augroup END

  " Plug 'gabrielelana/vim-markdown',         { 'as': 'gabrielelana-vim-markdown'}
    " let g:markdown_enable_mappings = 0
    " let g:markdown_enable_folding = 1
    " let g:markdown_enable_input_abbreviations = 0
endfunction

function s:PlugHaskell()
  Plug 'neovimhaskell/haskell-vim'
    let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
    let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
    let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
    let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
    let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
    let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
    let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
  Plug 'andy-morris/happy.vim'
  Plug 'andy-morris/alex.vim'

endfunction

function s:PlugCCpp()
  Plug 'octol/vim-cpp-enhanced-highlight'
  " Better highlighting for C/C++
    let g:cpp_posix_standard = 1
    let g:cpp_class_scope_highlight = 1
    let g:cpp_member_variable_highlight = 1
    let g:cpp_class_decl_highlight = 1
    let g:cpp_concepts_highlight = 1
endfunction

function s:PlugGo()
  Plug 'fatih/vim-go'
    " Disable features to let LSP configuration do the work
    let g:go_echo_go_info = 0
    let g:go_code_completion_enabled = 0
    let g:go_fmt_autosave = 0
    let g:go_gopls_enabled = 0
    let g:go_def_mapping_enabled = 0
endfunction

function s:PlugValgrind()
  Plug 'j-hui/valgrind.vim'
    let g:valgrind_arguments='--leak-check=yes --num-callers=64'
    function ValgrindHook()
      nmap [v <Plug>ValgrindStackUp
      nmap ]v <Plug>ValgrindStackDown
    endfunction
    augroup valgrind_hook
      autocmd!
      autocmd User ValgrindEnter call ValgrindHook()
    augroup END
endfunction

function s:PlugZig()
  Plug 'ziglang/zig.vim'
    let g:zig_fmt_autosave = 0
endfunction

function s:PlugMisc()
  Plug 'leanprover/lean.vim'
  Plug 'idris-hackers/idris-vim'
  Plug 'LnL7/vim-nix'
  Plug 'blyoa/vim-promela-syntax'
  Plug 'chrisbra/csv.vim'
  Plug 'rust-lang/rust.vim'
    let g:rust_fold = 1
  Plug 'leafgarland/typescript-vim'
  Plug 'keith/swift.vim'
  Plug 'dag/vim-fish'
  Plug 'cespare/vim-toml'
  Plug 'adborden/vim-notmuch-address',      { 'for': 'mail' }
  Plug 'neomutt/neomutt.vim'
endfunction

function plugins#filetypes#setup()
  call s:PlugCoq()
  call s:PlugLatex()
  call s:PlugMarkdown()
  call s:PlugHaskell()
  call s:PlugCCpp()
  call s:PlugGo()
  call s:PlugValgrind()
  call s:PlugZig()
  call s:PlugMisc()
  return []
endfunction

" vim: set ts=2 sw=2 tw=80 et :

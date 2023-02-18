scriptencoding utf-8
function pokerus#commands#setup()

  " :Qa (quality assurance for my typos) {{{
  command! Qa qa
  " }}}

  " Over80/120: highlight characters past 80/120 {{{
  command! Over80 normal! m`/\%>80v./+<CR>``
  command! Over120 normal! m`/\%>120v./+<CR>``
  " }}}

  " Refresh {{{
  function! s:refresh()
    silent! call mkdir(fnamemodify(tempname(), ':p:h'), '', 0700)
    set nohlsearch
    redraw
    redrawstatus
  endfunction
  command! -bang Refresh call s:refresh()
  " }}}

  " Trim trailing spaces {{{
  command! -range Trim <line1>,<line2> substitute/\s\+$//g | noh | normal! ``
  " }}}

  " Go to location of file {{{
  command! Here cd %:h
  " }}}

  " Print the location of file {{{
  command! Where echom expand('%')
  " }}}

  " Source .vimrc {{{
  command! Vimrc source ~/.vimrc
  " }}}

  " "Basic" mode: no mouse interaction, line numbers, or sign column {{{
  " Useful for copying and pasting from buffers as text
  let s:basicmode = 0
  function! s:basicToggle()
    if s:basicmode
      set mouse=a nu rnu signcolumn=yes
      let s:basicmode = 0
    else
      set mouse= nonu nornu signcolumn=no
      let s:basicmode = 1
    endif
  endfunction
  command! Basic call s:basicToggle()
  " }}}

  " "Share" mode: toggle share mode {{{
  " Function defined in settings.vim
  command! Share call g:PokerusToggleShare()
  " }}}

  " AutoFormat: format paragraph on each keystroke {{{
  command! AutoFormat \ if stridx(&formatoptions, 'a') == -1
                      \|  set formatoptions+=a
                      \|else
                      \|  set formatoptions-=a
                      \|endif
  " }}}

  " Modeline {{{
  function! AppendModeline()
    let l:modeline = printf(' vim: set ts=%d sw=%d tw=%d %set :',
      \ &tabstop, &shiftwidth, &textwidth, &expandtab ? '' : 'no')
    let l:modeline = substitute(&commentstring, '%s', l:modeline, '')
    call append(line('$'), l:modeline)
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

  " SyntaxGroup: add missing commas to BibTeX file {{{
  function s:show_syntax_group()
    echo 'hi<' . synIDattr(synID(line('.'),col('.'),1),'name') . '> trans<'
          \ . synIDattr(synID(line('.'),col('.'),0),'name') . '> lo<'
          \ . synIDattr(synIDtrans(synID(line('.'),col('.'),1)),'name') . '>'
  endfunction
  command! SyntaxGroup call <SID>show_syntax_group()
  " }}}

  " PDF: Open PDF file {{{
    function s:openPDF(...)
      if a:0 == 0
        let l:pdf = expand('%:r') . '.pdf'
      else
        let l:pdf = a:1
      endif
      echom 'Opening ' . l:pdf . '...'
      if has('mac')
        call jobstart('open ' . l:pdf)
      elseif has('linux')
        call jobstart('xdg-open ' . l:pdf)
      else
        echom 'Unsupported operating system (neither mac nor linux)'
      endif
    endfunction
    command! -nargs=? -complete=file PDF call <SID>openPDF(<f-args>)
  " }}}

endfunction
" vim: set ts=2 sw=2 tw=80 et foldmethod=marker foldlevel=0 :

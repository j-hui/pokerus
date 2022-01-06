syntax sync fromstart

" From https://stackoverflow.com/questions/6738902/vim-syntax-highlighting-with-and-lstlistings-lstinline/21651323#21651323
" Does not seem to work when placed in ~/.vim/after/syntax/tex/listings.vim
syn region texZone start="\\begin{lstlisting}" end="\\end{lstlisting}\|%stopzone\>"
syn region texZone  start="\\lstinputlisting" end="{\s*[a-zA-Z/.0-9_^]\+\s*}"
syn match texInputFile "\\lstinline\s*\(\[.*\]\)\={.\{-}}" contains=texStatement,texInputCurlies,texInputFileOpt

set nocompatible

" geegaws
syntax enable
filetype plugin indent on
set showcmd
set incsearch

" indentation
set autoindent
set sw=4
set softtabstop=4

" Disable cursor blink in graphical mode
set guicursor=a:blinkoff0
" Disable toolbar
set guioptions-=T

set textwidth=70
" Don't autowrap
set formatoptions-=tc
" Avoid leaving single-character words at end of line
set formatoptions+=1

set autoread

set dictionary=/usr/share/dict/words

" M-q to reformat current paragraph in normal or visual modes, à lá
" Emacs.  Can't map this in visual mode unfortunately.  Need to call
" this '<Esc>q' for the terminal and '<M-q>' for the GUI.
nnoremap <Esc>q gqap
nnoremap <M-q> gqap

" Enable mouse usage in terminals.
set mouse=a

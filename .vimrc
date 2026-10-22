" Copyright © 2003, 2009, 2026 Michael Shields
"
" Licensed under the Apache License, Version 2.0 (the "License");
" you may not use this file except in compliance with the License.
" You may obtain a copy of the License at
"
"     http://www.apache.org/licenses/LICENSE-2.0
"
" Unless required by applicable law or agreed to in writing, software
" distributed under the License is distributed on an "AS IS" BASIS,
" WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
" See the License for the specific language governing permissions and
" limitations under the License.

set nocompatible
set encoding=utf-8
scriptencoding utf-8

" geegaws
syntax enable
filetype plugin indent on
set showcmd
set incsearch

" indentation
set autoindent
set sw=4
set softtabstop=4

set wrap
set showbreak=⸗
set list
let g:shields_tab_required_filetypes = ['go', 'gomod', 'gowork', 'make', 'automake']

if $TERM_PROGRAM ==# 'iTerm.app' || $COLORTERM =~# 'truecolor\|24bit'
    set termguicolors
endif

let s:unprinted_match = 4711
let s:trailing_match = 4712
let s:nbsp_match = 4713

" SpecialKey colors every cell that 'listchars' draws for a tab or a no-break
" space, in every window, so it has no attributes, to let tabs be blank.
" Window-local matches color the markers and control characters that show. The
" values are explicit because Vim restores its default colors to a cleared
" group whenever 'background' changes.
function! s:WhitespaceColors() abort
    highlight ShieldsWhitespace guifg=#365773 guibg=#e8f4ff ctermfg=24 ctermbg=195 gui=NONE cterm=NONE
    highlight ShieldsNoBreakSpace guifg=#949494 guibg=NONE ctermfg=246 ctermbg=NONE gui=NONE cterm=NONE
    highlight SpecialKey guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE gui=NONE cterm=NONE
    highlight! link NonText ShieldsWhitespace
    highlight EndOfBuffer guifg=#365773 guibg=NONE ctermfg=24 ctermbg=NONE gui=NONE cterm=NONE
endfunction

function! s:WhitespaceWindow() abort
    let hide_tabs = index(g:shields_tab_required_filetypes, &filetype) >= 0
                \ || !get(b:, 'shields_show_tabs', 1)
    let &l:listchars = 'tab:' . (hide_tabs ? '  ' : '⇥ ')
                \ . ',trail:␣,nbsp:⍽,extends:…,precedes:…'
    let present = map(getmatches(), {_, m -> m.id})
    for id in [s:unprinted_match, s:trailing_match, s:nbsp_match]
        if index(present, id) >= 0
            call matchdelete(id)
        endif
    endfor
    if &l:list
        " Characters that Vim shows as ^X or <xx> instead of themselves.
        let unprinted = '\%(\p' . (hide_tabs ? '\|\t' : '') . '\)\@!.'
        call matchadd('ShieldsWhitespace', '\m' . unprinted, -1, s:unprinted_match)
        " Runs of spaces and no-break spaces that end the line; tabs stay out so
        " that blank ones stay blank. Atomic, and consuming the character before
        " the run ahead of \zs so that matching starts only at the beginning of
        " a run: a long run before text is then scanned once, not from each of
        " its characters. A negative lookahead and ".", not a negated
        " collection, match that character: the backtracking engine's collection
        " consumes only the base of a character with combining marks, leaving
        " them where the run must begin. A separate match, because the one
        " above can end right before a run and so take that character.
        call matchadd('ShieldsWhitespace',
                    \ '\m\%(^\|\%([ \u00a0\u202f]\)\@!.\)\zs'
                    \ . '\%([ \u00a0\u202f]\+\)\@>\ze[ \t\u00a0\u202f]*$',
                    \ -1, s:trailing_match)
        " Below the matches above, so that trailing ones take their colors.
        call matchadd('ShieldsNoBreakSpace', '\m[\u00a0\u202f]', -2, s:nbsp_match)
    endif
endfunction

function! s:WhitespaceMarkers() abort
    for window in getwininfo()
        call win_execute(window.winid, 'call s:WhitespaceWindow()')
    endfor
endfunction

augroup shields_whitespace
    autocmd!
    autocmd ColorScheme * call s:WhitespaceColors()
    autocmd FileType,BufWinEnter,WinNew * call s:WhitespaceMarkers()
    autocmd OptionSet list call s:WhitespaceMarkers()
augroup END
call s:WhitespaceColors()
call s:WhitespaceMarkers()

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

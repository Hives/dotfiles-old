" https://github.com/wincent/wincent/blob/a2ac58636942b92ddd2f4480ec6753d37ceeadcc/roles/dotfiles/files/.vim/autoload/wincent/functions.vim#L2
function! functions#spell() abort
    if has ('syntax')
        setlocal spell
        setlocal spellfile=~/.vim/spell/en.utf-8.add
        setlocal spelllang=en_gb
    endif
endfunction

" https://github.com/wincent/wincent/blob/a2ac58636942b92ddd2f4480ec6753d37ceeadcc/roles/dotfiles/files/.vim/autoload/wincent/functions.vim#L11
function! functions#plaintext() abort
    setlocal nolist                   " don't show whitespace while typing
    setlocal wrap                     " visually wrap long lines
    setlocal wrapmargin=0             " don't break long lines
    " setlocal textwidth=0              " don't break long lines
    " nnoremap <buffer> j gj
    " nnoremap <buffer> k gk

    setlocal textwidth=80             " do break long lines

    call functions#spell()            " activate spell checker

    " Ideally would keep 'list' set, and restrict 'listchars' to just show
    " whitespace errors, but 'listchars' is global and I don't want to go through
    " the hassle of saving and restoring.
    if has('autocmd')
        autocmd BufWinEnter <buffer> match Error /\s\+$/
        autocmd InsertEnter <buffer> match Error /\s\+\%#\@<!$/
        autocmd InsertLeave <buffer> match Error /\s\+$/
        autocmd BufWinLeave <buffer> call clearmatches()
    endif

endfunction


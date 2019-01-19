"set clipboard=unnamedplus
set ls=2
set backupcopy=yes
set mouse=a
set ts=2 sts=2 sw=2 et
set bg=dark

call plug#begin('~/.vim/plugged')
Plug 'crusoexia/vim-monokai'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive' 
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-rsi'
Plug 'scrooloose/nerdtree'
Plug 'christoomey/vim-tmux-navigator'
call plug#end()

let mapleader=' '
nnoremap <leader>v :e! ~/.vimrc<CR>
nnoremap <leader>e :e! %:h<CR>
nnoremap <leader>p :Files<CR>
nnoremap <leader>~ :Files ~<CR>
nnoremap <leader>f :Ag<CR>
nnoremap <leader>b :Buffer<CR>
nnoremap <leader>/ :BLines<CR>
nnoremap <TAB> gt
nnoremap <S-TAB> gT

colorscheme monokai

function! DHUpdateDotFiles()
    echom 'Updating configs ...'
    execute "!git -C ~/code/dhilst commit -am 'update config' && git -C ~/code/dhilst push origin master"
    redraw!
endfunction
command DHUpdateDotFiles :call DHUpdateDotFiles()<CR>

" Workarround for bug in gnome-terminal
function Yank() range
    silent! normal gvy
    silent! execute "!echo " . shellescape(@", 1) . " | xsel -b"
    redraw!
endfunction
xnoremap <C-y> :call Yank()<CR>

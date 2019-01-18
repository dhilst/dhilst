set bg=dark
set ts=2 sts=2 sw=2 et
set ignorecase
set ls=2
set backupcopy=yes
set mouse=r
set clipboard=unnmaed

call plug#begin('~/.vim/plugged')
Plug 'crusoexia/vim-monokai'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive' 
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-rsi'
Plug 'scrooloose/nerdtree'
call plug#end()

let mapleader=' '
nnoremap <leader>v :tabe! ~/.vimrc<CR>
nnoremap <leader>e :e! %:<CR>
nnoremap <leader>p :Files<CR>
nnoremap <leader>f :Ag<CR>
nnoremap <leader>b :Buffer<CR>
nnoremap <leader>/ :BLines<CR>

nnoremap <TAB> gt
nnoremap <S-TAB> gT

colorscheme monokai

call plug#begin('~/.vim/plugged')
Plug 'crusoexia/vim-monokai'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-abolish'
"Plug 'vsushkov/vim-phpcs'
"Plug 'tpope/vim-rsi'
Plug 'scrooloose/nerdtree'
Plug 'christoomey/vim-tmux-navigator'
Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'w0rp/ale', { 'on': 'ALEToggle' }
"Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
"Plug 'nsf/gocode', { 'rtp': 'vim', 'do': '~/.vim/plugged/gocode/vim/symlink.sh' }
Plug 'alvan/vim-closetag'
Plug 'mattn/gist-vim'
"Plug 'rust-lang/rust.vim'
Plug 'sapphirecat/php-psr2-vim'
call plug#end()

"set clipboard=unnamedplus
set ls=2
set backupcopy=yes
set mouse=a
set ts=2 sts=2 sw=2 et
set bg=dark
set scrolloff=10
set incsearch
set clipboard=unnamedplus
set smartcase
set exrc

filetype plugin on
filetype plugin indent on

let mapleader=' '
nnoremap <leader>v :e! ~/.vimrc<CR>
nnoremap <leader>e :e! %:h<CR>
nnoremap <leader>p :Files<CR>
nnoremap <leader>~ :Files ~<CR>
nnoremap <leader>f :Ag<CR>
nnoremap <leader>g :Hg<CR>
nnoremap <leader>b :Buffer<CR>
nnoremap <leader>l :ALEToggle<CR>
nnoremap <leader>/ :BLines<CR>
nnoremap <TAB> gt
nnoremap <S-TAB> gT
noremap <silent> <leader><leader> :noh<return>

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


au FileType go,php set ts=4 sts=4 sw=4 et
au BufRead,BufNewFile *.gohtml set filetype=gohtmltmpl

let g:closetag_filenames = "*.html,*.xhtml,*.phtml,*.erb,*.jsx,*.tsx,*.htmldjango"
let g:closetag_xhtml_filenames = '*.jsx,*.tsx'

" Command for git grep
" - fzf#vim#grep(command, with_column, [options], [fullscreen])
command! -bang -nargs=* Hg
  \ call fzf#vim#grep(
  \   'hrep '.shellescape(<q-args>).' .', 0,
  \   {}, <bang>0)


command! -bang -nargs=* Og
  \ call fzf#vim#grep(
  \   'ogrep '.shellescape(<q-args>), 0,
  \   {}, <bang>0)
"
" Show trailing white spaces
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/

" Indent php switch statements
let g:PHP_vintage_case_default_indent = 1

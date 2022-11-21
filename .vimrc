language en_US.utf-8
call plug#begin('~/.vim/plugged')
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' } " fuzzy finder
Plug 'junegunn/fzf.vim' " fuzzy finder
" Plug 'tpope/vim-fugitive' " git integration
Plug 'tpope/vim-sensible' " emacs binding on command editning and sensible defaults
Plug 'tpope/vim-surround' " surround things
Plug 'tpope/vim-abolish'  " smarted substitution
Plug 'tpope/vim-commentary'  " smarted substitution
Plug 'scrooloose/nerdtree' " Better tree
Plug 'w0rp/ale', { 'on': 'ALEToggle' }
Plug 'whonore/Coqtail'
Plug 'osyo-manga/vim-over' " Live search and replace preview
Plug 'psf/black' " python linter
Plug 'matze/vim-move' " alt arrow moving;:w
Plug 'sickill/vim-monokai'
call plug#end()

filetype plugin on
filetype plugin indent on

colorscheme monokai

" ALE stuff

let g:ale_linters = {
      \   'python': ['mypy'],
      \   'ocaml': ['merlin', 'ols'],
      \ }

let g:ale_linters_explicit = 1
let g:ale_fix_on_save = 1
let g:ale_fixers = {
      \ 'python': ['autopep8', 'black'],
      \ 'ocaml': ['ocamlformat', 'ocp-indent', 'remove_trailing_lines', 'trim_whitespace'],
      \ '*': ['remove_trailing_lines', 'trim_whitespace'] }

" Change hightlight color for are
hi ALEError cterm=underline ctermbg=NONE ctermfg=Red

" ALE stuff end

" ## added by OPAM user-setup for vim / base ## 93ee63e278bdfc07d1139a748ed3fff2 ## you can edit, but keep this line
let s:opam_share_dir = system("opam config var share")
let s:opam_share_dir = substitute(s:opam_share_dir, '[\r\n]*$', '', '')

let s:opam_configuration = {}

function! OpamConfOcpIndent()
  execute "set rtp^=" . s:opam_share_dir . "/ocp-indent/vim"
endfunction
let s:opam_configuration['ocp-indent'] = function('OpamConfOcpIndent')

function! OpamConfOcpIndex()
  execute "set rtp+=" . s:opam_share_dir . "/ocp-index/vim"
endfunction
let s:opam_configuration['ocp-index'] = function('OpamConfOcpIndex')

function! OpamConfMerlin()
  let l:dir = s:opam_share_dir . "/merlin/vim"
  execute "set rtp+=" . l:dir
endfunction
let s:opam_configuration['merlin'] = function('OpamConfMerlin')

let s:opam_packages = ["ocp-indent", "ocp-index", "merlin"]
let s:opam_check_cmdline = ["opam list --installed --short --safe --color=never"] + s:opam_packages
let s:opam_available_tools = split(system(join(s:opam_check_cmdline)))
for tool in s:opam_packages
  " Respect package order (merlin should be after ocp-index)
  if count(s:opam_available_tools, tool) > 0
    call s:opam_configuration[tool]()
  endif
endfor
" ## end of OPAM user-setup addition for vim / base ## keep this line
" ## added by OPAM user-setup for vim / ocp-indent ## 70b62b72c3b73b39fa04cf0199e952a4 ## you can edit, but keep this line
if count(s:opam_available_tools,"ocp-indent") == 0
  source "/morespace/geckos/tezos/_opam/share/ocp-indent/vim/indent/ocaml.vim"
endif
" ## end of OPAM user-setup addition for vim / ocp-indent ## keep this line

" Workarround for bug in gnome-terminal
function! Yank() range
  silent! normal gvy
  silent! execute "!echo " . shellescape(@", 1) . " | xsel -b"
  redraw!
endfunction
xnoremap <C-y> :call Yank()<CR>

" --------
" KEYBINDS
" --------
let mapleader=' '
nnoremap <leader>v :e! ~/.config/nvim/init.vim<CR>
nnoremap <leader>e :e! %:h<CR>
nnoremap <leader>p :Files<CR>
nnoremap <leader>~ :Files ~<CR>
nnoremap <leader>F :Rg<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>l :ALEToggle<CR>
nnoremap <leader>/ :BLines<CR>
noremap <leader>n :ALENextWrap<CR>

let g:coqtail_noimap = 1
augroup coqMaps
  au!
  au FileType coq noremap <buffer> <F10> :CoqStop\|CoqStart<CR>
  au FileType coq noremap <buffer> <Down>  :CoqNext<CR>
  au FileType coq noremap <buffer> <Up>  :CoqUndo<CR>
  au FileType coq noremap <buffer> <C-j>  :CoqNext<CR>
  au FileType coq noremap <buffer> <C-K> :CoqUndo<CR>
  au FileType coq setlocal comments=sO:*\ -,mO:*\ \ ,exO:*),s1:(*,mb:*,ex:*)
augroup END

set nohls                      " Do not highlight searchs by default, is annoying
set showcmd                    " Show selection statistics, like characters, words, lines etc
set ls=2                       " Show the file name at status line
set backupcopy=yes             " Some watch commands have problem with how vim save files, this solves it
set mouse=a                    " Enable mouse
set ts=2 sts=2 sw=2 et         " My default tab sizes
set incsearch                  " Search while typing
set clipboard=unnamedplus      " Clipboard native integration
set ignorecase smartcase       " Smarter case for searching
set exrc                       " Enable .vimrc in the current folder
set scrolloff=0                " This fix an annoying bug when running
                               "   vim inside a terminal inside another vim
set fo+=r                      " Format options, add a comment when you press enter from a commented line
"set signcolumn=yes             " Always draw sign column. Prevent buffer moving when adding/deleting sign.

if !filereadable($HOME."/.vim/autoload/plug.vim")
  execute "!curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
endif

call plug#begin('~/.vim/plugged')
Plug 'crusoexia/vim-monokai'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-scriptease'
"Plug 'vsushkov/vim-phpcs'
"Plug 'tpope/vim-rsi'
Plug 'scrooloose/nerdtree'
Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'w0rp/ale', { 'on': 'ALEToggle' }
"Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
"Plug 'nsf/gocode', { 'rtp': 'vim', 'do': '~/.vim/plugged/gocode/vim/symlink.sh' }
Plug 'alvan/vim-closetag'
Plug 'mattn/gist-vim'
Plug 'tpope/vim-obsession'
"Plug 'rust-lang/rust.vim'
"Plug 'sapphirecat/php-psr2-vim'
Plug 'osyo-manga/vim-over'
Plug 'ervandew/supertab'
Plug 'Valloric/YouCompleteMe'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'psf/black'
Plug 'sheerun/vim-polyglot'
Plug 'mattn/webapi-vim'
Plug 'mitsuhiko/vim-jinja'
Plug 'dhilst/vim-ansible-execute-task'
"Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
"Plug 'reasonml-editor/vim-reason-plus'
"Plug 'autozimu/LanguageClient-neovim', {
"    \ 'branch': 'next',
"    \ 'do': 'bash install.sh',
"    \ }
Plug 'AndrewRadev/bufferize.vim'
Plug 'farmergreg/vim-lastplace'
Plug 'lambdalisue/doctest.vim'
call plug#end()

filetype plugin on
filetype plugin indent on

colorscheme monokai

" Command for git grep
" - fzf#vim#grep(command, with_column, [options], [fullscreen])
command! -bang -nargs=* Hg
      \ call fzf#vim#grep(
      \   'hrep '.shellescape(<q-args>).' .', 0,
      \   {}, <bang>0)


command! -bang -nargs=* Og
      \ call fzf#vim#grep(
      \   'ogrepcomplex '.shellescape(<q-args>).' .', 0,
      \   {}, <bang>0)

command! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \   '/home/dhilst/code/rgrep/target/release/rgrep .'.shellescape(<q-args>), 0,
      \   {}, <bang>0)
"
" Show trailing white spaces
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/

" VARIABLES
" ---------
"
"let g:python_host_prog = "/usr/bin/python2"
let g:python3_host_prog = "/home/dhilst/.nvim-venv/bin/python3"
let g:ale_python_pylint_options = "--init-hook='import sys; sys.path.append(\"./\")'"


" Indent php switch statements
let g:PHP_vintage_case_default_indent = 1

" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'
let g:UltiSnipsEditSplit = 'vertical'
" better key bindings for UltiSnipsExpandTrigger
set rtp+=~/.vim/UltiSnips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
"let g:UltiSnipsSnippetsDir=$HOME."/.vim/UltiSnips"
"let g:UltiSnipsSnippetDirectories=[g:UltiSnipsSnippetsDir]
"
let g:closetag_filenames = "*.html,*.xhtml,*.phtml,*.erb,*.jsx,*.tsx,*.htmldjango,*.html.tera"
let g:closetag_xhtml_filenames = '*.jsx,*.tsx'


let g:ale_reason_ls_executable  = "/home/dhilst/.local/bin/reason-language-server"


let g:ale_linters = {
      \   'php': ['php'],
      \   'python': ['mypy'],
      \   'rust': ['cargo', 'rls', 'rustc'],
      \   'ocaml': ['merlin', 'ols'],
      \   'typescript': ['eslint', 'standard', 'tslint', 'tsserver', 'typecheck', 'xo'],
      \ }


let g:ale_linters_explicit = 1
let g:ale_fix_on_save = 1
let g:ale_fixers = {
      \ 'go': ['gofmt', 'goimports'],
      \ 'python': ['autopep8', 'black'],
      \ 'rust': ['rustfmt'],
      \ 'ocaml': ['ocamlformat', 'ocp-indent', 'remove_trailing_lines', 'trim_whitespace'],
      \ 'reason': ['refmt', 'remove_trailing_lines', 'trim_whitespace'],
      \ 'javascript': ['prettier'],
      \ 'typescript': ['prettier'],
      \ 'css': ['prettier'],
      \ }

" oCaml stuff
" let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
" execute "set rtp+=" . g:opamshare . "/merlin/vim"

" ansible stuff
func! s:AnsibleAnswerInCurrentFolder()
  let files = []
  for file in glob("answers-*.yaml", v:false, v:true)
    try
      if match(file, 'answers-\d\+\.yaml') != -1
        call add(files, file)
      end
    catch /list index out of range/
    endtry
  endfor
  try
    return reverse(sort(files))[0]
  catch /list index out of range/
    return "answers_file_not_found"
  endtry
endfunc
let g:ansible_answers = "test/answers-simple.yaml -e @test/answers-slurm.yaml"
let g:ansible_execute_task_command = "ansible-playbook -v test/include_tasks.yaml -i inventory/test_hosts -e file=$FILE -e @".g:ansible_answers." --limit ansible-test1"
let g:ansible_execute_playbook_command = "ansible-playbook -v $FILE -i inventory/test_hosts --limit ansible-test1 -e @".g:ansible_answers

" oCaml stuff
let g:LanguageClient_serverCommands = {
    \ 'reason': ['/home/dhilst/.yarn/bin/ocaml-language-server'],
    \ }
    "\ 'reason': ['/home/dhilst/.local/bin/reason-language-server'],

" enable autocomplete
let g:deoplete#enable_at_startup = 1
" Keep buffer position when switching buffers https://stackoverflow.com/questions/4251533/vim-keep-window-position-when-switching-buffers
if v:version >= 700
  au BufLeave * let b:winview = winsaveview()
  au BufEnter * if(exists('b:winview')) | call winrestview(b:winview) | endif
endif

" Handy functions
func! RandString(n) abort
  return system("openssl rand -base64 ".a:n)
endfunc

func! s:findbuf(bufpat) abort
  echo "Search for ".a:bufpat
  redir @o
  silent! ls
  redir END
  let buffers = split(@o, "\n")
  call map(buffers, {_, val -> split(val, '\s\+')})
  let ids = map(copy(buffers), {_, val -> val[0]})
  let names = map(copy(buffers), {_, val -> val[2]})
  let idx = match(names, a:bufpat)
  if idx == -1
    return -1
  endif
  return idx[idx]
endfunc

func! s:interm(command) abort
  let bufid = s:findbuf(a:command)
  if  bufid == -1
    split a:command
    startinsert
    call termopen(a:command)
  else
    try
      execute "bdelete! ".bufid
    catch /No buffers were deleted/
    endtry
  endif
endfunc


func! s:fcmd(func) abort
  execute "command! ".a:func." :call ".a:func."()"
endfunc
command! -nargs=1 Fcmd :call s:fcmd(<args>)

function! DHUpdateDotFiles()
  echom 'Updating configs ...'
  call s:interm("git -C ~/code/dhilst commit -am 'update config' && git -C ~/code/dhilst push origin master")
endfunction
Fcmd "DHUpdateDotFiles"

" Workarround for bug in gnome-terminal
function! Yank() range
  silent! normal gvy
  silent! execute "!echo " . shellescape(@", 1) . " | xsel -b"
  redraw!
endfunction
xnoremap <C-y> :call Yank()<CR>

function! OpenUrlRange() range abort
  silent! normal gvy
  silent! execute "!firefox ".shellescape(@", 1)
  redraw!
endfunc
Fcmd "OpenUrlRange"

function! OpenUrlLine() abort
  let link = matchstr(getline("."), 'https\?://\S*')
  echo link
  if link != ""
    execute "!firefox ".shellescape(link, 1)
  end
endfunc
Fcmd "OpenUrlLine"

function! OpenPlugin() abort
  let line = getline(".")
  let match = matchlist(line, 'Plug '."'".'\(.\{-\}\)'."'")
  if match[0] != "" && match[1] != ""
    silent! execute "!firefox https://github.com/".match[1]
  endif
endfunc
Fcmd "OpenPlugin"

func! Pprint() range
  normal gvc
python3 <<EOF
import vim
from pprint import pformat
text = pformat(eval(vim.eval('@"')))
vim.command(f"let text = {repr(text)}")
EOF
  echo text
  call setreg('"', text) 
  put "
  normal kdd
endfunc

func Matchall(text, pattern) abort
  let count = 1
  let matches = []
  while 1
    let result = matchstr(a:text, a:pattern, 0, count)
    if result == ""
      break
    endif
    call add(matches, result)
    let count = count + 1
  endwhile
  return matches
endfunc

function! OpenBitbucketBranch() abort
  let branch = trim(getline("."))
  echo branch
  execute "!firefox ".shellescape("https://bitbucket.versatushpc.com.br/projects/OPENCATTUS/repos/deployment/browse?at=refs/heads/".branch, 1)
endfunc

function! OpenJiraTicketLine() abort
  let line = getline(".")
  let projects = "OPENCATTUS VXCAT"
  let keys = split(projects, " ")
  for k in keys
    for m in Matchall(line, k.'-\d\+')
      execute "!firefox https://jira.versatushpc.com.br/browse/".m
    endfor
  endfor
endfunc
Fcmd "OpenJiraTicketLine"
" OPENCATTUS-1 OPENCATTUS-2

" --------
" KEYBINDS
" --------
let mapleader=' '
nnoremap <C-q><C-q> :q!<CR>
nnoremap <C-w><C-q> :wqa!<CR>
nnoremap <leader>v :e! ~/.vimrc<CR>
nnoremap <leader>e :e! %:h<CR>
nnoremap <leader>p :Files<CR>
nnoremap <leader>~ :Files ~<CR>
nnoremap <leader>f :Ag<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>l :ALEToggle<CR>
nnoremap <leader>/ :BLines<CR>
noremap <leader>s :OverCommandLine<CR>
nnoremap <leader>S :UltiSnipsEdit<CR>
nnoremap <leader>q :wq!a<CR>
noremap <leader>n :ALENextWrap<CR>
noremap <C-w><C-w> :w!<CR>
inoremap <C-a> ^


"
"conflicted with ultisnip
"nnoremap <TAB> gt
"nnoremap <S-TAB> gT

noremap <C-x>; <ESC>:
nnoremap <expr> <F2> ':OverCommandLine %s/'.expand('<c-r><c-w>').'<CR>'
vnoremap <expr> <F2> ':OverCommandLine %s/'.expand('<c-r>').'<CR>'
nnoremap :%s/ <ESC>:OverCommandLine %s/<CR>



" movement 
noremap <C-j> <ESC>:wincmd j<CR>
noremap <C-h> <ESC>:wincmd h<CR>
noremap <C-k> <ESC>:wincmd k<CR>
noremap <C-l> <ESC>:wincmd l<CR>




"au BufWritePre *.re,*.rei call LanguageClient#textDocument_formatting_sync()
au FileType go,php,python setlocal ts=4 sts=4 sw=4 et
au BufRead,BufNewFile *.html.tera set filetype=htmljinja
au FileType yaml setlocal ts=2 sts=2 sw=2 et
au BufRead,BufNewFile *.gohtml set filetype=gohtmltmpl
au FileType go nnoremap <buffer> <F8> :GoBuild<CR>

au FileType yaml,yaml.ansible vmap <buffer> <F7> <Plug>AnsibleExecuteTask
au FileType yaml.ansible      nmap <buffer> <F8> <Plug>AnsibleExecuteFile
au FileType yaml              nmap <buffer> <F9> <Plug>AnsibleExecutePlaybook
au FileType vim               nnoremap <buffer> <F9> :so %<CR>

au BufNewFile *.md read ~/.vim/templates/post.md
"auto format for the lazy
au FileType javascript au BufWritePre <buffer> normal gg=G``
au FileType python noremap <buffer> <F8> :Doctest -o ELLIPSIS<CR>
au FileType python noremap <buffer> <F9> :!python3 %<CR>



"set clipboard=unnamedplus
set nohls
set showcmd
set ls=2
set backupcopy=yes
set mouse=a
set ts=2 sts=2 sw=2 et
set bg=dark
set incsearch
set clipboard=unnamedplus
set smartcase
set exrc
set scrolloff=0 " This fix an annoying bug when running vim inside a terminal inside another vim

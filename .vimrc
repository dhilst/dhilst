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
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
"Plug 'nsf/gocode', { 'rtp': 'vim', 'do': '~/.vim/plugged/gocode/vim/symlink.sh' }
Plug 'alvan/vim-closetag'
Plug 'mattn/gist-vim'
"Plug 'rust-lang/rust.vim'
Plug 'sapphirecat/php-psr2-vim'
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
call plug#end()

"set clipboard=unnamedplus
set nohls
set showcmd
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
  \   '/home/dhilst/code/rust/foo/target/release/rgrep . '.shellescape(<q-args>), 0,
  \   {}, <bang>0)
"
" Show trailing white spaces
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/

" VARIABLES
" ---------

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


let g:ale_linters = {
            \   'php': ['php'],
            \   'python': ['pylint', 'mypy'],
            \   'rust': ['cargo', 'rls', 'rustc'],
            \ }


let g:ale_fix_on_save = 1
let g:ale_fixers = {
      \ 'go': ['gofmt', 'goimports'],
      \ 'python': ['autopep8', 'black'],
      \ 'rust': ['rustfmt'],
      \ }

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
let g:ansible_answers = "answers-2019120317.yml"
" let g:ansible_execute_task_command = "ansible-playbook -v include_tasks.yaml -i inventory/test_hosts -e file=$FILE -e @".s:AnsibleAnswerInCurrentFolder()
let g:ansible_execute_task_command = "ansible -m include_tasks -a $FILE localhost"
let g:ansible_execute_playbook_command = "ansible-playbook -v $FILE -i inventory/test_hosts -e @".s:AnsibleAnswerInCurrentFolder()

" Keep buffer position when switching buffers https://stackoverflow.com/questions/4251533/vim-keep-window-position-when-switching-buffers
if v:version >= 700
  au BufLeave * let b:winview = winsaveview()
  au BufEnter * if(exists('b:winview')) | call winrestview(b:winview) | endif
endif

" Handy functions
func! s:interm(command) abort
  split
  wincmd j
  enew
  call termopen(a:command)
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
  if link != ""
    silent! execute "!firefox ".link
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

" --------
" KEYBINDS
" --------
let mapleader=' '
nnoremap <leader>v :e! ~/.vimrc<CR>
nnoremap <leader>e :e! %:h<CR>
nnoremap <leader>p :Files<CR>
nnoremap <leader>~ :Files ~<CR>
nnoremap <leader>f :Rg<CR>
nnoremap <leader>b :Buffer<CR>
nnoremap <leader>l :ALEToggle<CR>
nnoremap <leader>/ :BLines<CR>
noremap <leader>s :OverCommandLine<CR>
nnoremap <leader>S :UltiSnipsEdit<CR>
nnoremap <leader>q :wq!a<CR>
"
"conflicted with ultisnip
"nnoremap <TAB> gt
"nnoremap <S-TAB> gT
noremap <silent> <leader>n :noh<return>

noremap <C-x>; <ESC>:
nnoremap <expr> <F2> ':OverCommandLine %s/'.expand('<c-r><c-w>').'<CR>'
vnoremap <expr> <F2> ':OverCommandLine %s/'.expand('<c-r>').'<CR>'
nnoremap <silent> <F9> :so ~/.vimrc<CR>

au FileType go,php,python setlocal ts=4 sts=4 sw=4 et
au BufRead,BufNewFile *.html.tera set filetype=htmljinja
au FileType yaml setlocal ts=2 sts=2 sw=2 et
au BufRead,BufNewFile *.gohtml set filetype=gohtmltmpl
au FileType go nnoremap <buffer> <F8> :GoBuild<CR>

au FileType yaml,yaml.ansible vmap <buffer> <F7> <Plug>AnsibleExecuteTask
au FileType yaml.ansible      nmap <buffer> <F8> <Plug>AnsibleExecuteFile
au FileType yaml              nmap <buffer> <F9> <Plug>AnsibleExecutePlaybook

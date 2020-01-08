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


" Keep buffer position when switching buffers https://stackoverflow.com/questions/4251533/vim-keep-window-position-when-switching-buffers
if v:version >= 700
  au BufLeave * let b:winview = winsaveview()
  au BufEnter * if(exists('b:winview')) | call winrestview(b:winview) | endif
endif

" Handy functions
function! DHUpdateDotFiles()
    echom 'Updating configs ...'
    execute "!git -C ~/code/dhilst commit -am 'update config' && git -C ~/code/dhilst push origin master"
    redraw!
endfunction

command! DHUpdateDotFiles :call DHUpdateDotFiles()<CR>

" Workarround for bug in gnome-terminal
function! Yank() range
    silent! normal gvy
    silent! execute "!echo " . shellescape(@", 1) . " | xsel -b"
    redraw!
endfunction
xnoremap <C-y> :call Yank()<CR>


let g:ansible_execute_task_command = "ansible -m include_tasks -a $FILE -i inventory/test_hosts -e @answers-2019120317.yml sms"
" Executes the selected text as an ansible task. The command
" is gathered from g:ansible_execute_task_command. The responsibity
" of selecting the right amout of text is to the user, the selected
" text is copied to a temporary file and g:ansible_exexute_task_command
" is executed by replacing $FILE substring by the temporary file created
" before.
function! AnsibleExecuteTask() range abort
    silent! normal gvy
    let tempname = tempname()
    let lines = split(@", '\n')
    let res = writefile(lines, tempname)
    if res == -1
      throw "writefile failed " . tempname
    endif
    let command = substitute(g:ansible_execute_task_command, "$FILE", tempname, "")
    try
      execute "!".command
    finally
      silent! execute "!rm -f ".tempname
    endtry
endfunction
command! -range AnsibleExecuteTask :call AnsibleExecuteTask()

" Executes the current file by replacing $FILE in
" g:ansible_execute_task_command to the current opened
" buffer
function! AnsibleExecuteFile(file) abort
    let command = substitute(g:ansible_execute_task_command, "$FILE", a:file, "")
    execute "!".command
endfunction
command! AnsibleExecuteFile :call AnsibleExecuteFile(expand("%"))

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
au FileType go nnoremap <buffer> <F8> :GoRun<CR>
au FileType yaml.ansible vnoremap <buffer> <F7> <ESC>:AnsibleExecuteTask<CR>
au FileType yaml.ansible nnoremap <buffer> <F8> :AnsibleExecuteFile<CR>

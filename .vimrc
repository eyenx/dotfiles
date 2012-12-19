"no vim compatibility
set nocompatible
"autoreload vimrc
autocmd! bufwritepost .vimrc source %
"allow backspacing over everything in insert mode
set backspace=indent,eol,start
set backup		" keep a backup file
set history=700		 
set undolevels=700
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching

" Don't use Ex mode, use Q for formatting
map Q gq
map Q gqap

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" because mouse is mainstream
if has('mouse')
  set mouse=a
endif

"syntax highlightning
if &t_Co > 2 || has("gui_running")
  filetype plugin on
  syntax on
  set hlsearch
  set incsearch
  set ignorecase
  set smartcase
endif

set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround
set expandtab

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  filetype plugin indent on
  augroup vimrcEx
  au!
  " textwidth
  autocmd FileType text setlocal textwidth=78
  " autojump last known position
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  augroup END

else

  set autoindent		" always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif
" powerline
set laststatus=2 " Always show the statusline
set encoding=utf-8 " Necessary to show Unicode glyphs
set t_Co=256
set guifont=Dejavu\ Sans\ Mono\ for\ Powerline\ 8
let g:Powerline_symbols='fancy'
let g:Powerline_cache_dir="/tmp"

" set number line counting
set number

" set colorscheme
colorscheme darkblue

" set backupdir
set backupdir=~/.vim/bak/,/tmp
" plugins
autocmd FileType python set omnifunc=pythoncomplete#Complete
"
" mappings
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h
map <Leader>n <esc>:tabnext<CR>
map <Leader>p <esc>:tabprevious<CR>
vnoremap <Leader>s :sort<CR>
vnoremap < <gv 
vnoremap > >gv 

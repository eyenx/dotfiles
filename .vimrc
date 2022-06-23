"disable formatting when pasting
set pastetoggle=<F2>

"no vim compatibility
set nocompatible

"autoreload vimrc
autocmd! bufwritepost .vimrc source %

"allow backspacing over everything in insert mode
set backspace=indent,eol,start

"set backup		" keep a backup file
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
set mouse=a
filetype plugin indent on
set omnifunc=syntaxcomplete#Complete
syntax on
set hlsearch
set incsearch
set ignorecase
set smartcase

"wrapping like a boss
set wrap                " word wrap
set textwidth=0         " 
set lbr                 " line break
set display=lastline    " don't display @ with long paragraphs

"tabs
set tabstop=4
set softtabstop=2
set shiftwidth=2
set shiftround
set expandtab
set autoindent		" always set autoindenting on

"other stuff takeon somewhere from the net
set shortmess+=I        " disable the welcome screen
set complete+=k         " enable dictionary completion
set completeopt+=longest
set clipboard+=unnamed  " yank and copy to X clipboard
set showmatch           " show matching brackets (),{},[]
set mat=5               " show mathine brackets for 0.5 seconds


" set font
set guifont=Tamzen

" set number line counting
set number

" set colorscheme
colorscheme mocha

set directory=/tmp
set grepprg=grep\ -nH\ $*
let g:tex_flavor ="latex"

" mappings
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <C-h> <C-w>h

" implementet with standard vim tabs
map <C-n> <esc>:tabnext<CR>
map <C-m> <esc>:tabprevious<CR>
vnoremap <S-s> :sort<CR>
vnoremap < <gv 
vnoremap > >gv 

" fold
set foldmethod=indent
set nofoldenable

" lint
let g:vim_markdown_folding_disabled=1

" statusline
set statusline=
set statusline+=%#Debug#
set statusline+=%{FugitiveStatusline()}
set statusline+=%#String#
set statusline+=\ %f
set statusline+=%m
set statusline+=%=
set statusline+=%y
set statusline+=\ %{&fileencoding?&fileencoding:&encoding}
set statusline+=\[%{&fileformat}\]
set statusline+=\ %p%%
set statusline+=\ %l:%c

" COC Stuff
" use <c-space>for trigger completion
inoremap <silent><expr> <c-space> coc#refresh()

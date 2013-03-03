"disable formatting when pasting
set pastetoggle=<F2>
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
set mouse=a
"syntax highlightning
"pathogen
filetype off
execute pathogen#infect()
filetype plugin indent on
syntax on
set ofu=syntaxcomplete#Complete
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
set softtabstop=4
set shiftwidth=4
set shiftround
set expandtab
set autoindent		" always set autoindenting on

"other stuff takeon somewhere from the outernet
set shortmess+=I        " disable the welcome screen
set complete+=k         " enable dictionary completion
set completeopt+=longest
set clipboard+=unnamed  " yank and copy to X clipboard
set showmatch           " show matching brackets (),{},[]
set mat=5               " show mathine brackets for 0.5 seconds
"copy/pasting for intend
vmap <c-y> y:call system("xclip -i", getreg("\""))<CR>
nmap <c-v> :call setreg("\"",system("xclip -o"))<CR>p
imap <c-v> <Esc><c-v>a
"remap those omnimenu keys
function! OmniPopup(action)
    if pumvisible()
        if a:action == 'j'
            return "\<C-N>"
        elseif a:action == 'k'
            return "\<C-P>"
        endif
    endif
    return a:action
endfunction

inoremap <silent>j <C-R>=OmniPopup('j')<CR>
inoremap <silent>k <C-R>=OmniPopup('k')<CR>
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
"colorscheme darkblue
"colorscheme wombat
colorscheme grb256
"colorscheme solarized
"colorscheme jellybeans

" set backupdir
set backupdir=/tmp
set directory=/tmp
" plugins
" pydiction
let g:pydiction_location = '/usr/share/pydiction/complete-dict'
"vim-latexsuite
set grepprg=grep\ -nH\ $*
let g:tex_flavor ="latex"
"
" mappings
"
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <C-h> <C-w>h
" implementet with buftabs
map <C-n> <esc>:bnext<CR>
map <C-m> <esc>:bprevious<CR>
noremap <silent> <C-s> :w<CR>
inoremap <silent> <C-s> <esc><esc>:w<CR>
vnoremap <silent> <C-s> <esc><esc>:w<CR>
vnoremap <S-s> :sort<CR>
vnoremap < <gv 
vnoremap > >gv 

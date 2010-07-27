""""""""""""""""""""""""""
" Ankur Sethi's ~/.vimrc "
" get.me.ankur@gmail.com "
""""""""""""""""""""""""""

" This .vimrc should (hopefully) work on Mac OS X, Linux and Haiku.
" Compatible with Vim 7.2 and up.

""""""""""""""""""""
" General Settings "
""""""""""""""""""""
set nocompatible
syntax on
set noswapfile
set autoread
set backspace=2
set hidden
set ignorecase
set smartcase
set incsearch
set nobackup
set confirm
filetype on
filetype plugin on
set fileformats=unix,dos,mac
set scrolloff=5

" Apparently, some Vims don't have autochdir.
if exists("+autochdir")
	set autochdir
endif


""""""""""""
" Wildmenu "
""""""""""""
set wildmenu
set wildignore=*.dll,*.o,*.pyc,*.bak,*.exe,*.jpg,*.jpeg,*.png,*.gif,*$py.class
set wildmode=full

""""""""""""""
" Appearance "
""""""""""""""
set number
set showcmd
set laststatus=2
set ruler


"""""""""""""""""""
" Tabs and Spaces "
"""""""""""""""""""
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4
set autoindent
filetype plugin indent on


""""""""""""""""
" GUI Specific "
""""""""""""""""
if has("gui_running")
    colorscheme molokai
    set lines=40
    set columns=100
	
	" MacVim specific options.
	if has("gui_macvim")
        " set fuoptions=maxvert,maxhorz,background:Normal
        " au GUIEnter * set fullscreen
        " Turn off the annoying 'ding' and visual bell.
		set vb t_vb=
	endif

	set cursorline

	" No menus, toolbars.
	set guioptions-=m
	set guioptions-=T
endif


""""""""""""
" Mappings "
""""""""""""
map <F1> :AT <CR>
map <F2> \be
map <F5> :source $MYVIMRC <CR>
map <M-BS> <C-w>
map <C-e> <End>
map <C-a> <Home>

" Scroll by display lines instead of logical lines.
:noremap k gk
:noremap j gj

if has("macunix")
	map <D-[> :tabprevious <CR>
	map <D-]> :tabnext <CR>
else
	map <C-Left> :tabprevious <CR>
	map <C-Right> :tabnext <CR>
endif

" Navigating buffers.
map <C-Tab> :bn<CR>
map <C-S-Tab> :bp<CR>
map <M-Tab> <C-^>


" Settings for Ruby files.
autocmd BufNewFile,BufRead *.rb
    \ setlocal expandtab shiftwidth=2 softtabstop=2 tabstop=2


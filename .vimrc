call plug#begin()
Plug 'itchyny/lightline.vim'
Plug 'sheerun/vim-wombat-scheme'
call plug#end()

set encoding=utf-8
scriptencoding utf-8

set laststatus=2
set noshowmode

if !has('gui_running')
  set t_Co=256
endif

let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'fugitive', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component': {
      \   'readonly': '%{&filetype=="help"?"":&readonly?"⭤":""}',
      \   'modified': '%{&filetype=="help"?"":&modified?"+":&modifiable?"":"-"}',
      \   'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
      \ },
      \ 'component_visible_condition': {
      \   'readonly': '(&filetype!="help"&& &readonly)',
      \   'modified': '(&filetype!="help"&&(&modified||!&modifiable))',
      \   'fugitive': '(exists("*fugitive#head") && ""!=fugitive#head())'
      \ },
		\ 'separator': { 'left': "\ue0b0", 'right': "\ue0b2" },
		  \ 'subseparator': { 'left': "\ue0b1", 'right': "\ue0b3" }
      \ }

syntax on
set background=dark

set incsearch
set hlsearch
set ignorecase

set backspace=indent,eol,start

set shiftwidth=4
set tabstop=4
set smarttab
set autoindent
set smartindent

set showmatch

set nobackup
set nowritebackup
set noswapfile


if has('gui_running')
    set guifont=Inconsolata\ for\ Powerline\ 12
    set guioptions-=T
    set guioptions-=m
    set cursorline
    set lines=50 columns=100
endif

if has('gui_running')
    colorscheme wombat
else
    colorscheme elflord
endif

highlight ExtraWhitespace ctermbg=darkred guibg=#382424
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=#f92672
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/

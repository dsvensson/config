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
    set guifont=ProggyCleanTTSZ\ 12
    set guioptions-=T
    set guioptions-=m
    set cursorline
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

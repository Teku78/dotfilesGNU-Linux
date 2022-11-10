

" Main
source ~/.config/nvim/settings/statusLine.vim

syntax on                   " habilitar los colores
set number                  " Enumerar las lineas
set numberwidth=1           " Reducir el margen vertical entre los nuermos y a ventana
set showmatch               " Mostrar la apertura y cierre de un parentesis y los otros
set cursorline              " Resaltar la linea donde se esta editando
set cursorcolumn
filetype plugin on
set termguicolors           " Habilita el uso de esqemas de colores 256
set clipboard=unnamedplus   " Copiar y pegar desde el porta papeles del sistema
set showmode                " Ocultar la barra de estado
set encoding=utf8           " Uso de caracteres internacionales
set nobackup                " No crear archivos de backup
set mouse=a                 " Copiar texto sin los numeros de linea con el mouse
"set paste                   " Copiar y pegar respetando el formato del texto (el uso hace que la identacion no funcione)
set ve+=onemore             " Colocar el insert al despues del ultimo caracter
set cmdheight=1             " La barra de estado tendra la altura de una fila
set nowrap
set wildmode=longest,list,full
set listchars=tab:\·\ ,extends:,precedes:,trail:␣,eol:·,nbsp:x
set list
set laststatus=2           " Mostrar la barra de estado
set ruler
set scrolloff=5                " no permiter que el cursor se desplaze por N lineas abajo y arriba
set wildmenu

set matchpairs+=<:>
" Indent
set expandtab
set tabstop=4
set shiftwidth=4
set autoindent
set smartindent

" Search
set hlsearch                " Resaltado de las coincidencias
set incsearch               " Muestra las coincidencias en tiempo real
set ignorecase              " Omite los texto en camelCase
set smartcase

" Others
" Mostrar una barra bertical en vez de un bloque en todos los modos
"set guicursor=n:ver100-iCursor
"set guicursor=i:ver100-iCursor
"set guicursor=v:ver100-iCursor
"set guicursor=a:blinkwait5-blinkon5-blinkoff5

if has("autocmd")
    autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \ exe "normal g`\"" |
    \ endif
endif
" Evitar errores de colorscheme al usar vim en tmux
autocmd vimenter * hi Normal guibg=NONE ctermbg=NONE


" Poder deshacer cambios incluso despues de haber cerrado el archivo
set undofile
set undodir=~/.config/nvim/undodir

" Eliminar los epacios en blanco al final de cada linea
autocmd BufWritePre * %s/\s\+$//e

" CONFIG KEYS
" ===================================================
let mapleader = ","
nmap <silent><F2> :NERDTreeToggle<CR>

map <Down> <NOP>
map <UP> <NOP>
map <LEFT> <NOP>
map <RIGHT> <NOP>

" header bash script
nnoremap bs i#!/bin/bash/<ESC>o
" RUNS
map <F3> <ESC>:w<CR>:exec '!python' shellescape(@%, 1)
imap <F4> <ESC>:w<CR>:!bash %<CR>
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags

nnoremap <leader>e :e $MYVIMRC<CR>
nmap <leader>r :noh<CR>
map <leader>vp :VimuxPromptCommand<CR>
map <leader>vl :VimuxRunLastCommand<CR>
" Comentarios
command! -nargs=0 Sw w !sudo '-S' tee % > /dev/null
" Theme
colorscheme aurora
set background=dark

" Custom status bar

call plug#begin()
" Barra lateral para navegacion
Plug 'preservim/nerdtree'
Plug 'preservim/vimux'
" Barra de estado
"Plug 'vim-airline/vim-airline'
"Plug 'vim-airline/vim-airline-themes'
" Autocompletado de palabras
"Plug 'vim-scripts/AutoComplPop'
" Uso de la tecla TAB para autocompletar
Plug 'ervandew/supertab'
" Soporte para PowerShell windows
"Plug 'zigford/vim-powershell'
" Visalizacion de los colores en hexadecimal y otros
Plug 'lilydjwg/colorizer'
" Sintaxis para JavaScript
"Plug 'jelera/vim-javascript-syntax'
Plug 'neovimhaskell/haskell-vim'
Plug 'nvim-lua/completion-nvim'
"Plug 'vim-syntastic/syntastic'
"----Temas---
"Plug 'kyoz/purify', { 'rtp': 'vim' }
"Plug 'bluz71/vim-nightfly-guicolors'
"Plug 'NLKNguyen/papercolor-theme'
"Plug 'ashfinal/vim-colors-paper'
"Plug 'reewr/vim-monokai-phoenix'
"Plug 'connorholyday/vim-snazzy'
"Plug 'pineapplegiant/spaceduck'
call plug#end()

"let g:SuperTabContextDefaultCompletionType = "<c-n>"
let g:SuperTabDefaultCompletionType = "<c-n>"

let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords

let g:haskell_indent_disable = 1


augroup filetype_vim
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
augroup END



"##########################
"barstatus
"##########################

"au InsertEnter * hi statusline guifg=black guibg=#d7afff ctermfg=black ctermbg=magenta
"au InsertLeave * hi statusline guifg=black guibg=#8fbfdc ctermfg=black ctermbg=cyan
"hi statusline guifg=#ffffff guibg=#0f1419 ctermfg=black ctermbg=cyan

"hi InsertEnter guibg=#8fbfdc ctermfg=137 ctermbg=234
let g:currentmode={
       \ 'n'  : 'NORMAL ',
       \ 'v'  : 'VISUAL ',
       \ 'V'  : 'V·Line ',
       \ "\<C-V>" : 'V·Block ',
       \ 'i'  : 'INSERT ',
       \ 'R'  : 'R ',
       \ 'Rv' : 'V·Replace ',
       \ 'c'  : 'Command ',
       \}

set statusline=
set statusline+=\ %{toupper(g:currentmode[mode()])}
set statusline+=%{&modified?'[+]':''}
"set statusline+=\ %n       " buffer number
"set statusline+=%##
set statusline+=\ %t  " path to the file
"set statusline+=%1*\ ››
set statusline+=%=        " switch to the right side
set statusline+=%2*\%y    " File type
"set statusline+=-         " Separator
"set statusline+=%l         " Current line
set statusline+=\%3*\ln:\ %02l/%L\ (%1p%%)  " Line number / total lines, percentage of document
"set statusline+=%2p%%     " porcentage

hi User1 ctermfg=007 ctermbg=239 guibg=#adff80 guifg=#010409
hi User2 ctermfg=007 ctermbg=239 guibg=#b998f5 guifg=#010409
hi User3 ctermfg=007 ctermbg=239 guibg=#29c1dc guifg=#010409

" %L total lines
" \ for spaces
" %f path to the file

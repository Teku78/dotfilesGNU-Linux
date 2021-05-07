
"====================[Configuraciones basicas]======================
" Principales
syntax on                       " Activar los colores
set termguicolors               " Permite establecer los temas
set number                      " Mostrar numero de filas
set numberwidth=1               " Reducir el margen vertical entre la ventana y el numero de fila
set nowrap                      " No adaptar el texto al tamaño de la pantalla
set showmatch                   " Muestra el cierre de las llaves, corchetes, parentesis etc
set noshowmode                  " Oculta el modo actual
set cursorline                  " Resalta la linea en la que se encuentra el cursor
set encoding=utf8               " Permite caracteres internacionales

" Busqueda
set hlsearch                    " Resalta las palabras en las busquedas
set incsearch                   " Muestra en tiempo real las busquedas
set ignorecase                  " Hace caso omiso de camelCase

" Identacion
set tabstop=4
set autoindent                  " Al hacer intro mantiente la identacion con respecto a la liena anterior
set smartindent
filetype plugin indent on       " Identacion del codigo
set shiftwidth=4                " Espacios por identacion

" Otros
set nobackup                    " No guarda los ficheros de backup
set ff=unix
set mouse=a                    " Copia el texto sin el numero de las filas
set clipboard=unnamedplus 		" Copiar y pagar texto dentro de vim
"set paste 						" Hacer copy-paste respetando el formato del archivo
"set ruler						" Muestra en que columna se encuentra el cursor en barra inferior
set nocompatible

" Mostras una barra vertical en modo normal y visual
set guicursor=n:ver100-iCursor
set guicursor=i:ver100-iCursor
set guicursor=v:ver100-iCursor
" Persistencia de los cambios de hacer y deshacer, despuesde de cerara un archivo
set undofile
set undodir=~/.config/nvim/undodir

" Hace compatible los temas de vim en tmux
autocmd vimenter * hi Normal guibg=NONE ctermbg=NONE
"
"
set listchars=tab:\\ ,extends:,precedes:,trail:␣,eol:·
set list
autocmd BufWritePre * %s/\s\+$//e
"set wrap
"set linebreak
"set showbreak=↪
"set breakindent
" ======================[Configuracion del teclado]=======================


" Bloquear teclas de direccion en modo INSERT
imap <Up> <NOP>
imap <Down> <NOP>
imap <Left> <NOP>
imap <Right> <NOP>

" Bloquer teclas de direccion en modo NORMAL
map <UP> <NOP>
map <Down> <NOP>
map <Left> <NOP>
map <Right> <NOP>

imap <C-s> <esc>$a
vmap <C-s> <esc>$a
imap <C-a> <esc>0i
"map! <F1> <ESC>iholaquetal<CR>
"nmap normal mode map
"imap inset mode map
imap <A-h> <C-o>h
imap <A-j> <C-o>j
imap <A-k> <C-o>k
imap <A-l> <C-o>l



" Ejecutar scripts de python
imap <F8> <ESC>:w<CR>:exec '!python' shellescape(@%, 1)<CR>

" =========== Pluguins =========

call plug#begin()
" Barra lateral para navegacion
Plug 'preservim/nerdtree'
" Barra de estado
"Plug 'vim-airline/vim-airline'
"Plug 'vim-airline/vim-airline-themes'
" Autocompletado de palabras
Plug 'vim-scripts/AutoComplPop'
" Uso de la tecla TAB para autocompletar
Plug 'ervandew/supertab'
" Soporte para PowerShell windows
"Plug 'zigford/vim-powershell'
" Visalizacion de los colores en hexadecimal y otros
Plug 'lilydjwg/colorizer'
" Sintaxis para JavaScript
"Plug 'jelera/vim-javascript-syntax'
Plug 'neovimhaskell/haskell-vim'
"----Temas---
Plug 'kyoz/purify', { 'rtp': 'vim' }
Plug 'bluz71/vim-nightfly-guicolors'
Plug 'NLKNguyen/papercolor-theme'
Plug 'ashfinal/vim-colors-paper'
Plug 'reewr/vim-monokai-phoenix'
Plug 'connorholyday/vim-snazzy'
Plug 'pineapplegiant/spaceduck'
call plug#end()
" =========== Configuraciones del Tema ========
"
"set t_Co=256
colorscheme aurora
set background=dark
" ================== AirLine ==================
"
" Mostrar buffers abiertos (como pestañas)
let g:airline#extensions#tabline#enabled = 1
" Mostrar sólo el nombre del archivo
let g:airline#extensions#tabline#fnamemod = ':t'
" Cargar fuente Powerline y símbolos (ver nota)
let g:airline_powerline_fonts = 1
" Tema de AirLine
"let g:airline_theme='spaceduck'

" ============ Configuracion Barra lateral ====
"
" Cambia el directorio actual al nodo padre actual
let g:NERDTreeChDirMode = 2
"Abrir/cerrar NERDTree con alt+s
imap <A-s> :NERDTreeToggle<CR>


" =========== Configuracion Supertab ========
" Supertab Conf
let g:SuperTabDefaultCompletionType = '<C-n>'

" haskell plugin
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords


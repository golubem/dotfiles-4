"======================================================================
" Vim settings
"======================================================================

syntax on
set background=dark
set shell=/bin/zsh
set encoding=utf-8
"set spell                  " Spell check
set spelllang=en_us,ru_ru
set showcmd                " Shows which command is printed yet
set mouse=a                " Adds a mouse support to
set number                 " Displays line numbers
set incsearch              " Incremental search feature
set nohlsearch             " Prevent highlighting search results
set ignorecase
set smartcase              " Smart case in search sequence
set wrap                   " Wraps text
set autoread               " Autoread file if it was changed outside vim
set scrolljump=4           " Minimal number of lines to scroll whet the cursor gets off the screen
set scrolloff=4            " Minimal number of screen lines to keep above and below the cursor
set ruler                  " Shows the line and column number of cursor position
set hidden
set tw=79                  " Text width
set colorcolumn=80         " Column to prevent long lines in code
set modeline
set modelines=5

let mapleader="\<Space>"
syntax on
filetype on
filetype indent on
filetype plugin on

" Indents
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

" History
set history=64
set undolevels=128
set undofile
set undodir=~/.config/nvim/undodir/
set undolevels=1000
set undoreload=10000

" Buffers
nmap <leader>s<left>   :leftabove  vnew<cr>
nmap <leader>s<right>  :rightbelow vnew<cr>
nmap <leader>s<up>     :leftabove  new<cr>
nmap <leader>s<down>   :rightbelow new<cr>

noremap <tab> <c-w><c-w>

nnoremap <leader>x      :bp <bar> bd #<cr>
nnoremap <leader>n      :bn<cr>
nnoremap <leader>p      :bp<cr>

" Improved navigation on wrapped lines
nnoremap j gj
nnoremap k gk

"======================================================================
" SPECIAL VIM MAGIC
"======================================================================

" Switch to US layout on normal mode
let g:layout='us'
function! SetUsLayout()
    let g:layout=system('xkblayout-state print %s')
    silent ! xkblayout-state set 0
endfunction

function! RestoreLayout()
    if g:layout != 'us'
        silent ! xkblayout-state set 1
    endif
endfunction

autocmd InsertLeave * call SetUsLayout()
autocmd InsertEnter * call RestoreLayout()

" Save files which require root permission
cmap w!! %!sudo tee > /dev/null %

inoremap jj <Esc> " Esc is so far away without this mapping...

" Disable arrows
map <Up> <NOP>
map <Down> <NOP>
map <Left> <NOP>
map <Right> <NOP>
imap <up> <nop>
imap <down> <nop>
imap <left> <nop>
imap <right> <nop>

" Move visual blocks
vnoremap < <gv
vnoremap > >gv

" Shift-Enter
"imap  <CR><CR><Esc>-cc

" Expand visual region by pressing v
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)

"======================================================================
" Plugins
"======================================================================

if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.config/nvim/plugged')

" Completion plugins
function! DoRemote(arg)
  UpdateRemotePlugins
endfunction
Plug 'Shougo/deoplete.nvim', { 'do': function('DoRemote') }

"Plug 'Raimondi/delimitMate'                         " Auto close quotes and etc.
Plug 'SirVer/ultisnips'                             " Ultisnippets configuration

function! BuildComposer(info)
  if a:info.status != 'unchanged' || a:info.force
    !cargo build --release
    UpdateRemotePlugins
  endif
endfunction

Plug 'euclio/vim-markdown-composer', { 'do': function('BuildComposer') }

" Better feeling
Plug 'easymotion/vim-easymotion'      " Fast navigation using shortcuts
Plug 'terryma/vim-expand-region'      " Expanding visual mode using v
Plug 'godlygeek/tabular'              " Easy aligning
Plug 'vim-airline/vim-airline'        " Fancy status line as fuck
Plug 'vim-airline/vim-airline-themes' " Fancy themes for fancy status line
Plug 'majutsushi/tagbar'              " Tagbar

" Themes
Plug 'tomasr/molokai'
Plug 'squarefrog/tomorrow-night.vim'

" Languages extensions
Plug 'scrooloose/syntastic'      " Multi syntax
Plug 'eagletmt/neco-ghc'         " Haskell completion
Plug 'jvirtanen/vim-octave'      " Octave completion support
Plug 'vim-scripts/dbext.vim'     " Databases support
Plug 'lervag/vimtex'             " LaTeX
Plug 'tpope/vim-rails'           " Ruby on Rails support
Plug 'tpope/vim-endwise'         " wisely add 'end' in ruby, endfunction/endif/more in vim script
Plug 'kchmck/vim-coffee-script'  " Support of Coffee script
Plug 'vim-ruby/vim-ruby'         " Ruby
Plug 'cakebaker/scss-syntax.vim' " Sass syntax files
Plug 'mattn/emmet-vim'           " Make HTML usable
Plug 'fatih/vim-go'              " Full feature GO support
Plug 'Shougo/deoplete.nvim'
Plug 'zchee/deoplete-go', { 'do': 'make'}
Plug 'slim-template/vim-slim'

" Navigation
Plug 'scrooloose/nerdtree' " File explorer
Plug 'kien/ctrlp.vim'      " Fuzzy search for everything

" Uncategorized
Plug 'airblade/vim-gitgutter'          " Git command supports
Plug 'bronson/vim-trailing-whitespace' " Removes useless whitespaces
Plug 'tpope/vim-surround'              " Surround quotes, tags and other
Plug 'scrooloose/nerdcommenter'        " Comment tool


call plug#end()

colorscheme tomorrow-night

"======================================================================
" EasyAlign
"======================================================================

map  <Leader>f <Plug>(easymotion-bd-f)
nmap <Leader>f <Plug>(easymotion-overwin-f)

nmap s <Plug>(easymotion-overwin-f2)

" Move to line
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)

" Move to word
map  <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)

"======================================================================
" CtrlP
"======================================================================

let g:ctrlp_show_hidden=1

"=====================================================================
" Vimtex
"=====================================================================

let g:vimtex_view_method='zathura'
let g:vimtex_latexmk_progname='nvr'
let g:vimtex_view_general_viewer='zathura'

"======================================================================
" Deoplete
"======================================================================

let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']

set completeopt-=preview

"======================================================================
" UltiSnips
"======================================================================

let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

"======================================================================
" Syntastic
"======================================================================

let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1
let g:syntastic_cpp_include_dirs=['/usr/include/' ]
let g:syntastic_cpp_check_header=1
let g:syntastic_cpp_remove_include_errors=1
let g:syntastic_c_checkers=['make']
let g:syntastic_always_populate_loc_list=1
let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1
let g:syntastic_error_symbol='✗'
let g:syntastic_warning_symbol='⚠'
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*gbar
let g:syntastic_cpp_compiler='clang++'
let g:syntastic_cpp_compiler_options=' -std=c++11 -stdlib=libc++'

"======================================================================
" NERDTree
"======================================================================

nmap <leader>m :NERDTreeToggle<CR>
let  NERDTreeHighlightCursorline=1
let  NERDTreeIgnore=['.yardoc', 'pkg']

"======================================================================
" Golang
"======================================================================

let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_interfaces = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1

let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
let g:syntastic_mode_map = { 'mode': 'active', 'passive_filetypes': ['go'] }

"======================================================================
" Ruby
"======================================================================

autocmd Filetype ruby setlocal ts=2 sts=2 sw=2
autocmd Filetype coffee setlocal ts=2 sts=2 sw=2

let g:rubycomplete_buffer_loading = 1
let g:rubycomplete_rails = 1

"======================================================================
" Airline
"======================================================================
set laststatus=2


let g:airline#extensions#tabline#enabled=1
let g:airline_powerline_fonts = 1
let g:Powerline_symbols='unicode'

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_symbols.space = "\ua0"

set showcmd


"======================================================================
" Base settings
"======================================================================

colorscheme Tomorrow-Night
syntax      on
set         background=dark
set         shell=/bin/sh
set         encoding=utf-8
set         mouse=a
set         number
set         incsearch
set         wrap
set         autoread
set         linebreak
set         scrolljump=4
set         scrolloff=4
set         ruler
set         ignorecase
set         smartcase
set         clipboard=unnamedplus
set         hidden
set         modeline
set         modelines=5
set         tw=79
set         colorcolumn=80
set         incsearch
let         mapleader=","

if has("gui_running")
    set guifont=Hack\ Regular\ 9
    set guioptions-=m
    set guioptions-=T
    set guioptions-=r
    set guioptions-=L
endif

"======================================================================
" Tabs
"======================================================================

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

"======================================================================
" History
"======================================================================

set history=64
set undolevels=128
set undodir=~/.vim/undodir/
set undofile
set undolevels=1000
set undoreload=10000

"======================================================================
" Manage buffers
"======================================================================

" Open new buffers in split
nmap <leader>s<left>   :leftabove  vnew<cr>
nmap <leader>s<right>  :rightbelow vnew<cr>
nmap <leader>s<up>     :leftabove  new<cr>
nmap <leader>s<down>   :rightbelow new<cr>

" Tab between buffers
noremap <tab> <c-w><c-w>

" Tabs navigation
nnoremap X              :bp <bar> bd #<cr>
nnoremap T              :bn<cr>
nnoremap H              :bp<cr>

"======================================================================
" Plugins
"======================================================================
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs \
      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif
call plug#begin('~/.config/nvim/plugged')

Plug 'davidhalter/jedi-vim'            " Python completion
Plug 'Shougo/deoplete.nvim'            " Autocompletion
Plug 'vim-scripts/dbext.vim'           " Databases support
Plug 'junegunn/vim-easy-align'         " Aligning some equalities
Plug 'airblade/vim-gitgutter'          " Git command supports
Plug 'bronson/vim-trailing-whitespace' " Removes useless whitespaces
Plug 'SirVer/ultisnips'                " Snippets
Plug 'tpope/vim-fugitive'              " Git wrapper for vim
Plug 'scrooloose/syntastic'            " Multi syntax
Plug 'scrooloose/nerdtree'             " File explorer
Plug 'jistr/vim-nerdtree-tabs'         " Tabs support for NEERDTree
Plug 'xolox/vim-misc'                  " Tags
Plug 'kien/ctrlp.vim'                  " Fuzzy search for everything
Plug 'suan/vim-instant-markdown'       " Markdown with prewiew
Plug 'tpope/vim-surround'              " Surround quotes, tags and other
Plug 'scrooloose/nerdcommenter'        " Comment tool
Plug 'bling/vim-airline'               " Powerline
Plug 'eagletmt/neco-ghc'               " Haskell completion
Plug 'jvirtanen/vim-octave'            " Octave completion support
Plug 'lervag/vimtex'                   " LaTeX
Plug 'tpope/vim-rails'                 " Ruby on Rails support

call plug#end()

let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

"======================================================================
" EasyAlign
"======================================================================

xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
vnoremap <silent> <Enter> :EasyAlign<cr>

"======================================================================
" CtrlP
"======================================================================

let g:ctrlp_show_hidden=1
nmap <leader>b :CtrlPBuffer<cr>

"=====================================================================
" Vimtex
"=====================================================================

let g:vimtex_latexmk_progname = 'nvr'
let g:vimtex_view_method='zathura'
let g:vimtex_fold_enabled=0
let g:vimtex_latexmk_options='-pdf -shell-escape'
let g:vimtex_view_general_viewer='zathura'

"======================================================================
" Airline
"======================================================================

let g:airline#extensions#tabline#enabled=1
let g:airline_powerline_fonts=1
let g:airline_symbols={}
let g:airline_left_sep=''
let g:airline_left_alt_sep=''
let g:airline_right_sep=''
let g:airline_right_alt_=''
let g:airline_symbols.br=''
let g:airline_symbols.re=''
let g:airline_symbols.li=''

"======================================================================
" Autocompletion(Deoplete, Jedi)
"======================================================================

" Deoplete
set completeopt+=noinsert

let g:deoplete#enable_at_startup = 1

" Jedi(Python)
autocmd FileType python setlocal completeopt-=preview
autocmd FileType python setlocal omnifunc=jedi#completions
let g:jedi#completions_enabled = 0
let g:jedi#auto_vim_configuration = 0
let g:jedi#smart_auto_mappings = 0
let g:jedi#show_call_signatures = 0

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

nmap <leader>m :NERDTreeTabsToggle<CR>
let  NERDTreeHighlightCursorline=1
let  NERDTreeIgnore=['.yardoc', 'pkg']

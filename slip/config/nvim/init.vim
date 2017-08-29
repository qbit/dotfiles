call plug#begin('~/.vim/plugged')

Plug 'fatih/vim-go'
Plug 'airblade/vim-gitgutter'
Plug 'https://bitbucket.org/kisom/eink.vim.git'

call plug#end()

nmap <Leader>bi :PlugInstall<CR>
nmap <Leader>bu :PlugUpdate<CR>
nmap <Leader>bc :PlugClean<CR>

let $NVIM_TUI_ENABLE_TRUE_COLOR=1
" colorscheme base16-default-light
set encoding=utf8
set t_Co=256 "Explicitly tell vim that the terminal supports 256 colors"
colorscheme eink

set dir=~/.vim/swaps
set nolist
set lazyredraw
set mouse-=a

" GitGutter
let g:gitgutter_realtime = 1

" Syntastic
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']

nmap <silent> ,/ :let @/=""<CR>

noremap <Left>  <NOP>
noremap <Right> <NOP>
noremap <Up>    <NOP>
noremap <Down>  <NOP>

nmap <leader>2 :set list!<CR>
nmap <leader>3 :set nu!<CR>
nmap <leader>4 :set paste!<CR>

" text & mutt files
au BufNewFile,BufRead /tmp/mutt*,/tmp/cvs*,*.txt set noai noshowmatch
au BufNewFile,BufRead /tmp/mutt*,/tmp/cvs*,*.txt setlocal spell spelllang=en_us

au BufNewFile,BufRead /private/var/*/mutt* set noai noshowmatch
au BufNewFile,BufRead /private/var/*/mutt* setlocal spell spelllang=en_us

" git commits
au BufNewFile,BufRead *.git/COMMIT_EDITMSG set noai noshowmatch
au BufNewFile,BufRead *.git/COMMIT_EDITMSG setlocal spell spelllang=en_us

autocmd BufWritePre * %s/\s\+$//e

autocmd FileType go nmap <leader>b  <Plug>(go-build)
autocmd FileType go nmap <leader>r  <Plug>(go-run)

if has("autocmd")
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g'\"" | endif
endif


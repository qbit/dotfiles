set nocompatible
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

nmap <Leader>bi :PluginInstall<CR>
nmap <Leader>bu :PluginInstall!<CR>
nmap <Leader>bc :PluginClean<CR>

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'airblade/vim-gitgutter'
Plugin 'fatih/vim-go'
Plugin 'vim-syntastic/syntastic'

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

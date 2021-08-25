set shell=bash

call plug#begin('~/.vim/plugged')
     Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
     Plug 'ibhagwan/fzf-lua'
     Plug 'vijaymarupudi/nvim-fzf'
     Plug 'kyazdani42/nvim-web-devicons'
call plug#end()


{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.neovim;
in {
  options.profiles.user.neovim.enable = mkEnableOption "my neovim configuration";
  config = mkIf cfg.enable {
    programs.neovim = {
      enable = true;
      extraConfig = ''
        set bg=light
        set go=a
        set mouse=a
        set nohlsearch
        set clipboard=unnamed,unnamedplus

        " some basics
        nnoremap c "_c
        set nocompatible
        filetype plugin on
        syntax on
        set encoding=utf-8
        set number relativenumber
        " enable autocompletion
        set wildmode=longest,list,full
        " disable automatic commenting on newline
        autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

        " alias replace all to S
        nnoremap S :%s//g<Left><Left>

        " automatically delete all trailing whitespace on save
        autocmd BufWritePre * %s/\s\+$//e
      '';
    };
  };
}

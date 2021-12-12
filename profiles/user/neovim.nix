{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.neovim;
in {
  options.profiles.user.neovim.enable =
    mkEnableOption "my neovim configuration";
  config = mkIf cfg.enable {
    programs.neovim = {
      enable = true;
      extraConfig = ''
        set go=a
        set mouse=a
        set clipboard=unnamed,unnamedplus

        " some basics
        set nocompatible
        filetype indent plugin on
        syntax on
        set encoding=utf-8
        set number relativenumber
        " enable autocompletion
        set wildmode=longest,list,full
        " don't mess with long lines
        set formatoptions=qrn1lj

        " alias replace all to S
        nnoremap S :%s//g<Left><Left>

        " automatically delete all trailing whitespace on save
        autocmd BufWritePre * %s/\s\+$//e
      '';
    };
  };
}

{ config, pkgs, lib, options, ... }:

{
  imports = [ /etc/nixos/hardware-configuration.nix <home-manager/nixos> ];

  environment.etc."nixos/overlays-compat/overlays.nix".text =
    builtins.readFile ./pkgs/overlays.nix;

  nix = {
    # required for remote builders
    trustedUsers = [ "root" "@wheel" ];
    nixPath = options.nix.nixPath.default
      ++ [ "nixpkgs-overlays=/etc/nixos/overlays-compat/" ];
    # conserve disk space by hardlinking identical store files
    autoOptimiseStore = true;
  };

  # steam, etc
  nixpkgs = {
    config.allowUnfree = true;
    overlays = [
      (self: super: {
        base-scripts = (super.runCommand "base-scripts" {
          preferLocalBuild = true;
          allowSubstitutes = false;
        } ''
          for tool in ${./bin/tools}"/"*; do
            install -D -m755 $tool $out/bin/$(basename $tool)
          done

          patchShebangs $out/bin
        '');
      })
    ];
  };

  boot = {
    # Use the systemd-boot EFI boot loader.
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    # ntfs write support
    supportedFilesystems = [ "ntfs-3g" ];
  };

  networking = {
    firewall = {
      allowedTCPPorts = [
        22000 # syncthing transfer
      ];
      allowedUDPPorts = [
        21027 # syncthing discovery
      ];
    };

    networkmanager = {
      enable = true;
      # wifi.backend = "iwd";
    };
  };

  console = {
    # font = "Lat2-Terminus16";
    keyMap = "us";
  };

  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Chicago";

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  programs = {
    ssh.startAgent = true;
    fish.enable = true;
  };

  # users.mutableUsers = false;
  # Don't forget to set a password with ‘passwd’.
  users.users.lh = {
    isNormalUser = true;
    shell = pkgs.fish;
    extraGroups = [
      "wheel" # sudo
      "networkmanager" # networking
      "input" # uinput? steam controller?
      "video"
      "dialout" # serial ports for MCU programming
      "lp" # printing?
    ];
  };

  home-manager.useUserPackages = true;
  home-manager.users.lh = { config, pkgs, ... }: {
    # home-manager
    nixpkgs.config = {
      allowUnfree = true;
      # lutris and protontricks depend on this
      permittedInsecurePackages = [ "p7zip-16.02" ];
    };
    # nix-shell, etc
    xdg.configFile."nixpkgs/config.nix".text = ''
      {
        allowUnfree = true;
      }
    '';
    home = {
      packages = with pkgs; [
        # system-related
        # TODO package as utils only?
        # exfat # use exFAT-formatted drives
        ntfs3g # write to NTFS-formatted drives

        # CLI's
        zip
        unzip
        wget # file download
        ripgrep # better grep, needed for doom-emacs features
        fio # disk benchmarking
        rmlint # find and rm duplicate files
        youtube-dl # video download
        rclone # multiplatform cloud sync
        ffsend # firefox send client

        # language tools for emacs integration
        shellcheck # check shell scripts for syntax and semantics
        nixfmt # format nix files
        # broken
        # nix-diff

        # TUI's
        htop # system monitoring

        base-scripts
      ];
      sessionVariables = {
        # add doom commands to path
        PATH = "$PATH:$HOME/.emacs.d/bin/";
        # fall back to emacs if no emacs server
        EDITOR = "emacsclient -ca emacs";
        FILE = "nnn";

        # https://www.topbug.net/blog/2016/09/27/make-gnu-less-more-powerful/
        LESS =
          "--quit-if-one-screen --ignore-case --status-column --LONG-PROMPT --RAW-CONTROL-CHARS --HILITE-UNREAD --tabs=4 --no-init --window=-2";
        # less colors
        # https://unix.stackexchange.com/questions/119/colors-in-man-pages/147#147
        # https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
        LESS_TERMCAP_mb = "$(tput bold; tput setaf 2)"; # green
        LESS_TERMCAP_md = "$(tput bold; tput setaf 6)"; # cyan
        LESS_TERMCAP_me = "$(tput sgr0)";
        LESS_TERMCAP_so =
          "$(tput bold; tput setaf 3; tput setab 4)"; # yellow on blue
        LESS_TERMCAP_se = "$(tput rmso; tput sgr0)";
        LESS_TERMCAP_us = "$(tput smul; tput bold; tput setaf 7)"; # white
        LESS_TERMCAP_ue = "$(tput rmul; tput sgr0)";
        LESS_TERMCAP_mr = "$(tput rev)";
        LESS_TERMCAP_mh = "$(tput dim)";
        LESS_TERMCAP_ZN = "$(tput ssubm)";
        LESS_TERMCAP_ZV = "$(tput rsubm)";
        LESS_TERMCAP_ZO = "$(tput ssupm)";
        LESS_TERMCAP_ZW = "$(tput rsupm)";
      };
    };

    # restart obsolete services on rebuild
    systemd.user.startServices = true;

    programs = {
      # Let Home Manager install and manage itself.
      home-manager.enable = true;

      fish = {
        enable = true;
        loginShellInit = ''
          # fish does not perform wordsplitting
          set EDITOR emacsclient -ca emacs

          # fish-foreign-env does not handle $()
          set LESS_TERMCAP_mb (tput bold; tput setaf 2)
          set LESS_TERMCAP_md (tput bold; tput setaf 6)
          set LESS_TERMCAP_me (tput sgr0)
          set LESS_TERMCAP_so (tput bold; tput setaf 3; tput setab 4)
          set LESS_TERMCAP_se (tput rmso; tput sgr0)
          set LESS_TERMCAP_us (tput smul; tput bold; tput setaf 7)
          set LESS_TERMCAP_ue (tput rmul; tput sgr0)
          set LESS_TERMCAP_mr (tput rev)
          set LESS_TERMCAP_mh (tput dim)
          set LESS_TERMCAP_ZN (tput ssubm)
          set LESS_TERMCAP_ZV (tput rsubm)
          set LESS_TERMCAP_ZO (tput ssupm)
          set LESS_TERMCAP_ZW (tput rsupm)
        '';
        interactiveShellInit = ''
          fish_vi_key_bindings
          # Emulates vim's cursor shape behavior
          # Set the normal and visual mode cursors to a block
          set fish_cursor_default block
          # Set the insert mode cursor to a line
          set fish_cursor_insert line
          # Set the replace mode cursor to an underscore
          set fish_cursor_replace_one underscore
          # The following variable can be used to configure cursor shape in
          # visual mode, but due to fish_cursor_default, is redundant here
          set fish_cursor_visual block
        '';
        shellAbbrs = {
          mkd = "mkdir -pv";
          nrs = "sudo nixos-rebuild switch";
          nrsl = "sudo nixos-rebuild switch -option builders ''";
          nrsu = "sudo nix-channel --update; sudo nixos-rebuild switch";
        };
        functions = {
          ls.body = ''
            command ls --human-readable --literal --color=auto \
            --group-directories-first $argv
          '';
          grep.body = "command grep --color=auto $argv";
          diff.body = "command diff --color $argv";
          pgrep.body = "command pgrep --list-name --ignore-case $argv";
        };
      };

      bash = {
        enable = true;
        shellOptions = [
          # Append to history file rather than replacing
          "histappend"
          # extended globbing
          "extglob"
          "globstar"
          # warn if closing shell with running jobs
          "checkjobs"
          # cd by typing directory name alone
          "autocd"
        ];
        shellAliases = {
          mkd = "mkdir -pv";
          nrs = "sudo nixos-rebuild switch";
          nrsl = "sudo nixos-rebuild switch -option builders ''";
          nrsu = "sudo nix-channel --update; sudo nixos-rebuild switch";
          ls = "ls -hN --color=auto --group-directories-first";
          grep = "grep --color=auto";
          diff = "diff --color";
          yt =
            "youtube-dl --add-metadata -i -o '%(upload_date)s-%(title)s.%(ext)s'";
          yta = "yt -x -f bestaudio/best";
          ffmpeg = "ffmpeg -hide_banner";
          ffplay = "ffplay -hide_banner";
          ffprobe = "ffprobe -hide_banner";
        };
        initExtra = ''
          stty -ixon # disable ctrl-s and ctrl-q
          # https://wiki.archlinux.org/index.php/Bash/Prompt_customization
          export PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\W\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"
        '';
      };

      # vi mode for bash, other shells like python
      readline = {
        enable = true;
        variables = {
          show-mode-in-prompt = true;
          editing-mode = "vi";
        };
        bindings = {
          "\\e[A" = "history-search-backward";
          "\\e[B" = "history-search-forward";
        };
        extraConfig = ''
          $if term=linux
            set vi-ins-mode-string \1\e[?0c\2
            set vi-cmd-mode-string \1\e[?8c\2
          $else
            set vi-ins-mode-string \1\e[6 q\2
            set vi-cmd-mode-string \1\e[2 q\2
          $endif
          set keymap vi-command
          # these are for vi-command mode
          j: history-search-forward
          k: history-search-backward
        '';
      };

      neovim = {
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

      emacs = {
        enable = true;
        extraPackages = epkgs: [ epkgs.emacs-libvterm ];
      };

      git = {
        enable = true;
        userName = "hpfr";
        userEmail = "44043764+hpfr@users.noreply.github.com";
        extraConfig = {
          url."git@github.com:".pushInsteadOf =
            "git://github.com,https://github.com";
        };
      };
    };

    services.syncthing.enable = true;
  };
}

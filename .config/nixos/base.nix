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
  nixpkgs.config.allowUnfree = true;

  boot = {
    # Use the systemd-boot EFI boot loader.
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    # TODO exfat support not needed with 5.4?
    extraModulePackages = [ config.boot.kernelPackages.exfat-nofuse ];
    # ntfs write support
    supportedFilesystems = [ "ntfs-3g" ];
  };

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.wireless.networks = {
  #   eduroam = {
  #     auth = ''
  #       key_mgmt=WPA-EAP
  #       eap=PEAP
  #       identity="user@example.com"
  #       password="secret"
  #     '';
  #   };
  # };
  networking.networkmanager.enable = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  console = {
    # font = "Lat2-Terminus16";
    keyMap = "us";
  };

  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Chicago";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # environment.systemPackages = with pkgs; [
  #   hplip # python27Packages.dbus-python dbus
  # ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  programs.ssh.startAgent = true;

  # Open ports in the firewall.
  networking.firewall = {
    allowedTCPPorts = [
      22000 # syncthing transfer
    ];
    allowedUDPPorts = [
      21027 # syncthing discovery
    ];
  };

  services = {
    # Enable CUPS to print documents.
    # printing = {
    #   enable = true;
    #   drivers = [ pkgs.hplipWithPlugin ]; # FIXME hp-setup not working
    # };

    # emacs = {
    #   enable = true;
    #   package = with pkgs;
    #     ((emacsPackagesNgGen emacs).emacsWithPackages (epkgs: [
    #       epkgs.emacs-libvterm
    #     ]));
    # };
  };

  # users.mutableUsers = false;
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.lh = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "input" "video" "lp" ];
  };

  home-manager.useUserPackages = true;
  home-manager.users.lh = { config, pkgs, ... }: {
    nixpkgs.config.allowUnfree = true;
    home = {
      packages = with pkgs; [
        # system-related
        gnumake
        gnutls # for circe
        zip
        unzip
        socat # detach processes
        exfat # use exFAT-formatted drives
        ntfs3g # write to NTFS-formatted drives

        # CLI's
        fio # disk benchmarking
        ripgrep # better grep, needed for doom-emacs features
        wget # file download
        youtube-dl # video download
        rclone # multiplatform cloud sync
        ffsend # firefox send client

        # environments and related
        texlive.combined.scheme-medium # latex environment
        pandoc # convert document formats
        python3
        shellcheck # check shell scripts for syntax and semantics
        nix-diff
        nixfmt # format nix files

        # TUI's
        nnn # file manager
        htop # system monitoring
        ncdu # disk management
      ];
      # sourced by .profile
      sessionVariables = {
        # add .local/bin/ and all subdirectories to path
        PATH = ''
          $PATH:$HOME/.emacs.d/bin/:$(du "$HOME/.local/bin/" | cut -f2 | tr '\n' ':' | sed 's/:*$//')
        '';
        # fall back to emacs if no emacs server
        EDITOR = "emacsclient -ca emacs";
        TERMINAL = "alacritty";
        BROWSER = "firefox";
        READER = "zathura";
        FILE = "nnn";
        # use this variable in scripts to generalize dmenu, rofi, etc
        MENU = "rofi -dmenu";
        SUDO_ASKPASS = "$HOME/.local/bin/tools/menupass";

        # GTK2_RC_FILES = "$HOME/.config/gtk-2.0/gtkrc-2.0";
        # for adwaita-qt
        QT_STYLE_OVERRIDE = "Adwaita-Dark";

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

    programs = {
      # Let Home Manager install and manage itself.
      home-manager.enable = true;

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
          pg = "pgrep";
          pk = "pkill";
          mkd = "mkdir -pv";
          mpv = "mpv --input-ipc-server=/tmp/mpvsoc$(date +%s)";
          nrs = "sudo nixos-rebuild switch";
          nrsu = "sudo nix-channel --update; sudo nixos-rebuild switch";
          SS = "sudo systemctl";
          dots = "git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME";
          f = "$FILE";
          e = "$EDITOR";
          x = "sxiv -ft *";
          sdn = "sudo shutdown -h now";
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
          export PS1=export PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\W\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"
        '';
      };

      readline = {
        enable = true;
        variables.editing-mode = "vi";
        extraConfig = ''
          $if mode=vi
            set show-mode-in-prompt on
            set vi-ins-mode-string \1\e[6 q\2
            set vi-cmd-mode-string \1\e[2 q\2
          $endif
        '';
      };

      neovim = {
        enable = true;
        extraConfig = ''
          set bg=light
          set go=a
          set mouse=a
          set nohlsearch
          set clipboard+=unnamedplus

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

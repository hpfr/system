{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.base;
in {
  options.profiles.user.base.enable =
    mkEnableOption "my user base configuration";

  config = mkIf cfg.enable {
    profiles.user = {
      bash.enable = true;
      readline.enable = true;
      fish.enable = true;
      emacs.enable = true;
      neovim.enable = true;
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
        # TODO: package as utils only? for relabeling
        # exfat # use exFAT-formatted drives
        ntfs3g # write to NTFS-formatted drives

        # CLI's
        zip
        unzip
        wget # file download
        ripgrep # better grep, needed for doom-emacs features
        fio # disk benchmarking
        pciutils # lspci
        usbutils # lsusb
        rmlint # find and rm duplicate files
        youtube-dl # video download
        rclone # multiplatform cloud sync
        ffsend # firefox send client

        # language tools, useful for emacs integration
        shellcheck # check shell scripts for syntax and semantics
        nixfmt # format nix files
        jq # json parsing
        # broken
        # nix-diff

        # TUI's
        htop # system monitoring

        base-scripts
      ];
      sessionVariables = {
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

      # cross-shell prompt with many integrations
      starship = {
        enable = true;
        settings.character.symbol = "🧮";
      };

      git = {
        enable = true;
        userName = "hpfr";
        userEmail = "44043764+hpfr@users.noreply.github.com";
        # I'm not sure if this works
        extraConfig = {
          url."git@github.com:".pushInsteadOf =
            "git://github.com,https://github.com";
        };
      };
    };

    services.syncthing.enable = true;
  };
}

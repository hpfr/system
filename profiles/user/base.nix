{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.base;
in {
  options.profiles.user.base.enable =
    mkEnableOption "my user-level base configuration";

  config = mkIf cfg.enable {
    profiles.user = {
      bash.enable = true;
      readline.enable = true;
      fish.enable = true;
      emacs.enable = true;
      neovim.enable = true;
    };

    home = {
      stateVersion = "21.03";
      packages = with pkgs; [
        # CLI's
        zip
        unzip
        wget # file download
        fd # better find
        bat # better cat
        ripgrep # better grep
        ripgrep-all # pdf's, etc
        file # get filetypes
        fio # disk benchmarking
        recode # change file encoding in place
        nmap # network discovery and security auditing
        pciutils # lspci
        usbutils # lsusb
        ddcutil # change monitor settings
        evtest # debug integrated devices
        rmlint # find and rm duplicate files
        ssh-audit # audit ssh configuration
        youtube-dl # video download
        rclone # multiplatform cloud sync

        # nix-diff # broken
        nix-du
        # nix-tree

        # TUI's
        htop # system monitoring

        base-scripts
      ];
      sessionVariables = {
        # https://www.topbug.net/blog/2016/09/27/make-gnu-less-more-powerful/
        LESS =
          "--quit-if-one-screen --ignore-case --status-column --LONG-PROMPT --RAW-CONTROL-CHARS --HILITE-UNREAD --tabs=4 --no-init --window=-2";
      };
    };

    # restart obsolete services on rebuild
    systemd.user.startServices = true;

    programs = {
      # Let Home Manager install and manage itself.
      home-manager.enable = true;

      man = {
        enable = true;
        generateCaches = true; # M-x man completion
      };

      # cross-shell prompt with many integrations
      starship = {
        enable = true;
        settings.character.symbol = "🧮";
      };

      git = {
        enable = true;
        package = pkgs.gitAndTools.gitFull;
        userName = "Liam Hupfer";
        userEmail = "liam@hpfr.net";
        delta.enable = true; # better diff highlighting
        extraConfig = {
          # magit forge
          github.user = "hpfr";
          gitlab.user = "hpfr";
          # sendemail.suppressFrom = true;
          # I'm not sure if this works
          url."git@github.com:".pushInsteadOf =
            "git://github.com,https://github.com";
        };
      };
    };
  };
}

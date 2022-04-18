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
      username = "lh";
      homeDirectory = "/home/${config.home.username}";
      stateVersion = "21.05";
      packages = with pkgs; [
        # CLI's
        libarchive # archival and compression
        wget # file download
        fd # better find
        du-dust # better du
        tokei # repo metrics
        scc # more repo metrics
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
        git-annex # git beyond plain text
        git-annex-utils # git-annex disk usage (gadu)
        metastore # filesystem metadata file format
        difftastic # semantic diff
        diffoscope # advanced file diff tool
        rmlint # find and rm duplicate files
        trash-cli # manage XDG trash
        ssh-audit # audit ssh configuration
        exiftool # work with photo exif data
        yt-dlp # video download
        rclone # multiplatform cloud sync
        entr # run commands when files change
        archivemount # mount archives as filesystems

        nix-diff
        nix-du
        nix-tree
        nixos-option

        base-scripts

        foot.terminfo
      ];

      # https://www.topbug.net/blog/2016/09/27/make-gnu-less-more-powerful/
      sessionVariables.LESS =
        "--quit-if-one-screen --ignore-case --status-column --LONG-PROMPT --RAW-CONTROL-CHARS --HILITE-UNREAD --tabs=4 --no-init --window=-2";
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

      git = {
        enable = true;
        package = pkgs.gitAndTools.gitFull;
        userName = "Liam Hupfer";
        userEmail = "liam@hpfr.net";
        delta.enable = true; # better diff highlighting
        extraConfig = {
          pull.rebase = true;
          # magit forge
          github.user = "hpfr";
          gitlab.user = "hpfr";
          # sendemail.suppressFrom = true;
          # I'm not sure if this works
          url."git@github.com:".pushInsteadOf =
            "git://github.com,https://github.com";
        };
      };

      direnv = {
        enable = true;
        nix-direnv.enable = true;
      };

      # cross-shell prompt with many integrations
      starship = {
        enable = true;
        settings = {
          add_newline = false;
          character = {
            success_symbol = "🧮";
            error_symbol = "[✘](bold red)";
          };
        };
      };

      # better cat
      bat.enable = true;

      # better top
      bottom.enable = true;
    };
  };
}

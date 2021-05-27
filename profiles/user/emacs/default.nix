{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.emacs;
in {
  options.profiles.user.emacs.enable = mkEnableOption "my emacs configuration";

  config = mkIf cfg.enable {
    programs = {
      emacs = {
        enable = true;
        extraPackages = epkgs: [ epkgs.vterm ];
      };
      fish.shellInit = ''
        # fish does not perform wordsplitting
        set EDITOR emacsclient -ca emacs
      '';
    };

    services.emacs = {
      enable = false;
      # TODO: make this independent of emacs service upstream
      client.enable = true; # xdg desktop file
    };

    home = {
      packages = with pkgs; [
        # TODO: expose these only to emacs somehow
        gnutls # circe
        sqlite # org-roam
        ripgrep # doom code searching features
        jq # json parsing
        libreoffice # docx to docview
        gdb # gdb mode, lsp gdb
        nodejs # dap-mode
        openssl # elpher gemini support

        # language linting and formatting
        shellcheck # shell linting
        shfmt # shell formatting
        clang-tools # for clang-format
        nixfmt # opinionated nix formatting
        html-tidy # html and xml formatting
        black # python

        # language servers
        ccls # c
        lua53Packages.digestif # TeX
        gopls # go
        jdk11 # java
        rls # rust
        nodePackages.pyright # python
        python3Packages.debugpy
        nodePackages.javascript-typescript-langserver
      ];

      sessionVariables = {
        # add doom commands to path
        PATH = "$PATH:$HOME/.config/emacs/bin/";
        # fall back to emacs if no emacs server
        EDITOR = "emacsclient -ca emacs";
      };
    };

    xdg = {
      configFile = {
        "doom" = {
          source = ./doom;
          recursive = true;
        };
        "doom/doom-source-dir.el".text = ''
          (setq lh/doom-source-dir "${toString ./doom}/")
        '';
      };
      mimeApps = let
        applyToAll = list:
          builtins.listToAttrs (map (key: {
            name = key;
            value = "emacsclient.desktop";
          }) list);
      in {
        associations.added = applyToAll [ "application/pdf" "inode/directory" ]
          // {
            "x-scheme-handler/gopher" = "emacsclient-elpher.desktop";
            "x-scheme-handler/gemini" = "emacsclient-elpher.desktop";
          };
        defaultApplications = applyToAll [ "application/pdf" "inode/directory" ]
          // {
            "x-scheme-handler/gopher" = "emacsclient-elpher.desktop";
            "x-scheme-handler/gemini" = "emacsclient-elpher.desktop";
          };
      };
      dataFile.emacsclient = {
        target = "applications/emacsclient.desktop";
        text = ''
          [Desktop Entry]
          Type=Application
          Name=Emacsclient
          GenericName=Text Editor
          Comment=Edit text
          MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
          # The -n option is necessary to actually get a new frame for some reason
          # from terminal you don't need it but you do from openers like firefox
          # https://forum.manjaro.org/t/emacsclient-as-desktop-application/132072
          Exec=emacsclient -cna 'emacs' %F
          Icon=emacs
          Type=Application
          Terminal=false
          Categories=Development;TextEditor;
          StartupWMClass=Emacs
          Keywords=Text;Editor;
        '';
        emacsclient-elpher = {
          target = "applications/emacsclient-elpher.desktop";
          text = ''
            [Desktop Entry]
            Type=Application
            Name=Elpher
            GenericName=Gopher/Gemini browser in Emacs
            Comment=Open gopher/gemini link in Elpher
            MimeType=x-scheme-handler/gemini;x-scheme-handler/gopher;
            Exec=emacsclient -create-frame --alternate-editor="" --no-wait --eval '(progn (x-focus-frame nil) (elpher-go "%u"))'
            Icon=emacs
            Type=Application
            Terminal=false
            Categories=Network;Gemini;Gopher;
            StartupWMClass=Emacs
            NoDisplay=True
          '';
        };
      };
    };
  };
}

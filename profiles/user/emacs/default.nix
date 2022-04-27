{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.emacs;
in {
  options.profiles.user.emacs.enable = mkEnableOption "my emacs configuration";

  config = mkIf cfg.enable {
    programs = {
      emacs = {
        enable = true;
        package = pkgs.emacsNativeComp;
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
      packages = with pkgs;
        [
          # TODO: expose these only to emacs somehow
          gnutls # circe
          gcc # roam, magit forge https://www.reddit.com/r/emacs/comments/adlmh2/
          perl # magit features like rebase remove commit
          sqlite # dash docsets
          ripgrep # doom code searching features
          jq # json parsing
          gdb # gdb mode, lsp gdb
          nodejs # dap-mode
          openssl # elpher gemini support
          python3 # treemacs advanced features

          # language linting and formatting
          (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
          sdcv # dictionary lookup
          languagetool # grammar
          jdk # languagetool
          shellcheck # shell linting
          shfmt # shell formatting
          nixfmt # opinionated nix formatting
          html-tidy # html and xml formatting
        ] ++ optionals config.profiles.user.gui-base.enable [
          # docx to docview
          libreoffice
          # TODO: replace with symbols nerd font
          emacs-all-the-icons-fonts
          # promnesia
        ] ++ (with python3Packages; [ pip ]);

      file = {
        ".aspell.conf".text = ''
          variety w_accents
          personal nc/config/aspell/en.pws
          repl nc/config/aspell/en.prepl
        '';
        # sane-default class for latex documents and Org export
        "texmf/tex/latex/my-article/my-article.cls".source = ./my-article.cls;
      };

      # need the first for pip
      sessionPath = [ "$HOME/.local/bin" "${config.xdg.configHome}/emacs/bin" ];
      sessionVariables = {
        # fall back to emacs if no emacs server
        EDITOR = "emacsclient -ca emacs";
      };
    };

    systemd.user = {
      timers.promnesia-index = {
        Unit.After = [ "graphical-session.target" ];
        Timer = {
          OnCalendar = "hourly";
          Unit = "promnesia-index.service";
          Persistent = true;
        };
        Install.WantedBy = [ "timers.target" ];
      };
      services = {
        promnesia-index = {
          Unit = {
            After = [ "graphical-session.target" ];
            Wants = [ "promnesia-index.timer" ];
          };
          Service.ExecStart = "/home/lh/.local/bin/promnesia index";
        };
        promnesia-serve = {
          Unit.After = [ "graphical-session.target" ];
          Install.WantedBy = [ "graphical-session.target" ];
          Service.ExecStart = "/home/lh/.local/bin/promnesia serve";
        };
      };
    };

    xdg = {
      configFile = {
        "latexmk/latexmkrc".text = ''
          $emulate_aux = 1;
          $aux_dir = '.latexmk-build';
        '';
        "doom" = {
          source = ./doom;
          recursive = true;
        };
        "doom/doom-source-dir.el".text = ''
          (setq lh/doom-source-dir "${toString ./doom}/")
        '';
        "doom/modules/lh/email/mu4e-load-path.el".text = ''
          (cl-pushnew "${pkgs.mu}/share/emacs/site-lisp/mu4e" load-path)
        '';
        "doom/languagetool-server-jar.el".text = ''
          (after! langtool
            (setq langtool-language-tool-server-jar "${pkgs.languagetool}/share/languagetool-server.jar"))
        '';

        # https://github.com/karlicoss/promnesia/blob/master/doc/config.py
        "promnesia/config.py".text = ''
          from promnesia.common import Source
          from promnesia.sources import auto

          ''''
          List of sources to use.

          You can specify your own, add more sources, etc.
          See https://github.com/karlicoss/promnesia#setup for more information
          ''''
          SOURCES = [
              Source(
                  auto.index,
                  '~/exocortex',
                  name='exocortex'
              ),
              Source(
                  auto.index,
                  '~/nc/bookmarks.org',
                  name='bookmarks dump'
              )
          ]
        '';
      };
      mimeApps = let
        applyToAll = list:
          builtins.listToAttrs (map (key: {
            name = key;
            value = "emacsclient.desktop";
          }) list);
      in {
        associations.added = applyToAll [
          "application/pdf"
          "inode/directory"
          "text/x-diff"
          "application/epub+zip"
          "application/vnd.ms-powerpoint"
        ] // {
          "x-scheme-handler/mailto" = "emacsclient-mail.desktop";
          "x-scheme-handler/gopher" = "emacsclient-gemini.desktop";
          "x-scheme-handler/gemini" = "emacsclient-gemini.desktop";
        };
        defaultApplications = applyToAll [
          "application/pdf"
          "inode/directory"
          "text/x-diff"
          "application/epub+zip"

          # override the original Emacs desktop entry
          "text/english"
          "text/plain"
          "text/x-makefile"
          "text/x-c++hdr"
          "text/x-c++src"
          "text/x-chdr"
          "text/x-csrc"
          "text/x-java"
          "text/x-moc"
          "text/x-pascal"
          "text/x-tcl"
          "text/x-tex"
          "application/x-shellscript"
          "text/x-c"
          "text/x-c++"
        ] // {
          "x-scheme-handler/mailto" = "emacsclient-mail.desktop";
          "x-scheme-handler/gopher" = "emacsclient-gemini.desktop";
          "x-scheme-handler/gemini" = "emacsclient-gemini.desktop";
        };
      };
      # these are output to /etc/profiles/per-user/lh/share/applications
      desktopEntries = {
        emacsclient-gemini = let
          execScript = pkgs.writeShellScript "emacsclient-gemini.sh" ''
            set -euo pipefail
            emacsclient --create-frame --alternate-editor 'emacs --eval' --no-wait --eval "(progn (x-focus-frame nil) (require 'elpher) (elpher-go \"$1\"))"
          '';
        in {
          type = "Application";
          name = "Elpher";
          genericName = "Gopher/Gemini browser";
          comment = "View Gopher/Gemini sites";
          mimeType = [ "x-scheme-handler/gemini" "x-scheme-handler/gopher" ];
          exec = "${execScript} %u";
          categories = [ "Network" "X-Gemini" "X-Gopher" ];
          settings.NoDisplay = "true";
        };
        emacsclient-capture = {
          type = "Application";
          name = "Capture";
          genericName = "org-capture interface";
          comment = "Capture tasks";
          mimeType = [ "text/plain" ];
          exec = "org-capture";
          categories = [ "Utility" "TextTools" "ProjectManagement" ];
        };
        # https://github.com/karlicoss/open-in-editor
        emacsclient-promnesia = {
          type = "Application";
          name = "Promnesia Editor";
          genericName = "Promnesia editor:// scheme handler";
          comment = "View Promnesia-indexed files";
          mimeType = [ "x-scheme-handler/editor" ];
          exec =
            "python ${config.home.homeDirectory}/repos/open-in-editor/open_in_editor.py --editor emacs %u";
          categories = [ "Utility" "TextTools" ];
          settings.NoDisplay = "true";
        };
      };
    };
  };
}

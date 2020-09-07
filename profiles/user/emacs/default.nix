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
      fish.loginShellInit = ''
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
        shellcheck # shell linting
        nixfmt # opinionated nix formatting
        html-tidy # html and xml formatting
        libreoffice # docx to docview
      ];

      sessionVariables = {
        # add doom commands to path
        PATH = "$PATH:$HOME/.emacs.d/bin/";
        # fall back to emacs if no emacs server
        EDITOR = "emacsclient -ca emacs";
      };
    };

    xdg = {
      configFile."doom" = {
        source = ./doom;
        recursive = true;
      };
      mimeApps.associations.added."application/pdf" = "emacsclient.desktop";
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
      };
    };
    services.picom.opacityRule = [ "92:class_g = 'Emacs'" ];
  };
}
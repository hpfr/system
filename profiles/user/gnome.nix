{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.gnome;
in {
  options.profiles.user.gnome.enable =
    mkEnableOption "my user-level GNOME configuration";
  config = mkIf cfg.enable {
    profiles.user = {
      wayland-base.enable = true;
      # gnome console
      foot.enable = false;
    };

    home.sessionVariables.TERMINAL = "kgx";

    # glib has gsettings
    systemd.user.services.bgcron.Service.Environment = "PATH=${
        with pkgs;
        lib.makeBinPath [ coreutils libnotify glib xdg-user-dirs ]
      }";

    # Disable gnome-keyring ssh-agent in favor of the default agent
    xdg = {

      # TODO: add safeeyes once it works on gnome (widget issue)
      configFile."autostart/gnome-keyring-ssh.desktop".text = ''
        ${fileContents
        "${pkgs.gnome.gnome-keyring}/etc/xdg/autostart/gnome-keyring-ssh.desktop"}
        Hidden=true
      '';
      mimeApps = {
        # GSConnect writes this every time if we don't add it in both default and added
        associations.added = {
          "x-scheme-handler/sms" =
            "org.gnome.Shell.Extensions.GSConnect.desktop";
          "x-scheme-handler/tel" =
            "org.gnome.Shell.Extensions.GSConnect.desktop";
        };
        defaultApplications = let
          applyToAll = list:
            builtins.listToAttrs (map (key: {
              name = key;
              value = "org.gnome.FileRoller.desktop";
            }) list);
        in applyToAll [
          "application/bzip2"
          "application/gzip"
          "application/vnd.android.package-archive"
          "application/vnd.ms-cab-compressed"
          "application/vnd.debian.binary-package"
          "application/x-7z-compressed"
          "application/x-7z-compressed-tar"
          "application/x-ace"
          "application/x-alz"
          "application/x-ar"
          "application/x-archive"
          "application/x-arj"
          "application/x-brotli"
          "application/x-bzip-brotli-tar"
          "application/x-bzip"
          "application/x-bzip-compressed-tar"
          "application/x-bzip1"
          "application/x-bzip1-compressed-tar"
          "application/x-cabinet"
          "application/x-cd-image"
          "application/x-compress"
          "application/x-compressed-tar"
          "application/x-cpio"
          "application/x-chrome-extension"
          "application/x-deb"
          "application/x-ear"
          "application/x-ms-dos-executable"
          "application/x-gtar"
          "application/x-gzip"
          "application/x-gzpostscript"
          "application/x-java-archive"
          "application/x-lha"
          "application/x-lhz"
          "application/x-lrzip"
          "application/x-lrzip-compressed-tar"
          "application/x-lz4"
          "application/x-lzip"
          "application/x-lzip-compressed-tar"
          "application/x-lzma"
          "application/x-lzma-compressed-tar"
          "application/x-lzop"
          "application/x-lz4-compressed-tar"
          "application/x-ms-wim"
          "application/x-rar"
          "application/x-rar-compressed"
          "application/x-rpm"
          "application/x-source-rpm"
          "application/x-rzip"
          "application/x-rzip-compressed-tar"
          "application/x-tar"
          "application/x-tarz"
          "application/x-tzo"
          "application/x-stuffit"
          "application/x-war"
          "application/x-xar"
          "application/x-xz"
          "application/x-xz-compressed-tar"
          "application/x-zip"
          "application/x-zip-compressed"
          "application/x-zstd-compressed-tar"
          "application/x-zoo"
          "application/zip"
          "application/zstd"
        ] // {
          "x-scheme-handler/sms" =
            "org.gnome.Shell.Extensions.GSConnect.desktop";
          "x-scheme-handler/tel" =
            "org.gnome.Shell.Extensions.GSConnect.desktop";
        };
      };
    };

    home.packages = with pkgs.gnome;
      with pkgs.gnomeExtensions; [
        dconf-editor

        sound-output-device-chooser
        emoji-selector
        just-perfection
        random-wallpaper
      ];

    # not necessary in a desktop environment
    services.network-manager-applet.enable = lib.mkForce false;

    # declaratively manage gsettings
    dconf = {
      enable = true;
      # https://rycee.gitlab.io/home-manager/options.html#opt-dconf.settings
      settings = {
        "org/gnome/gnome-session".logout-prompt = false;
        # workspaces span displays
        "org/gnome/mutter" = {
          workspaces-only-on-primary = false;
          dynamic-workspaces = false;
        };
        "org/gnome/desktop/wm/preferences".num-workspaces = 4;
        "org/gnome/desktop/interface" = {
          clock-show-weekday = true;
          color-scheme = "prefer-dark";
          cursor-theme = "capitaine-cursors";
          cursor-size = 32;
        };
        # compose key
        "org/gnome/desktop/input-sources".xkb-options =
          [ "terminate:ctrl_alt_bksp" "compose:ralt" ];
        "org/gnome/shell/app-switcher".current-workspace-only = true;
        "org/gnome/desktop/privacy".remove-old-trash-files = true;
        "org/gnome/shell".enabled-extensions = [
          "sound-output-device-chooser@kgshank.net"
          "emoji-selector@maestroschan.fr"
          "drive-menu@gnome-shell-extensions.gcampax.github.com" # drive menu icon in activities bar
          "gsconnect@andyholmes.github.io"
        ];
        "org/gnome/shell".favorite-apps = [
          "emacsclient.desktop"
          "firefox.desktop"
          "foot.desktop"
          "org.keepassxc.KeePassXC.desktop"
          "com.nextcloud.desktopclient.nextcloud.desktop"
        ];
        # keybindings
        "org/gnome/settings-daemon/plugins/media-keys" = {
          screensaver = [ "<Super>z" ];
          suspend = [ "<Super><Shift>z" ];
          power = [ "<Super>x" ];
          # there's no reboot dconf key?
          # reboot = [ "<Super><Shift>x" ];
        };
        "org/gnome/shell/keybindings" = {
          toggle-overview = [ ];
          # bound to super+num, which I want for workspace switching
          switch-to-application-1 = [ ];
          switch-to-application-2 = [ ];
          switch-to-application-3 = [ ];
          switch-to-application-4 = [ ];
          switch-to-application-5 = [ ];
          switch-to-application-6 = [ ];
          switch-to-application-7 = [ ];
          switch-to-application-8 = [ ];
          switch-to-application-9 = [ ];
        };
        "org/gnome/mutter/keybindings" = {
          toggle-tiled-left = [ "<Super>Left" "<Super>h" ];
          toggle-tiled-right = [ "<Super>Right" "<Super>l" ];
        };
        "org/gnome/desktop/wm/keybindings" = {
          close = [ "<Super>q" ];
          minimize = [ "<Super><Control>q" ];
          maximize = [ "<Super>Up" "<Super>k" ];
          unmaximize = [ "<Super>Down" "<Super>j" ];
          toggle-fullscreen = [ "<Super>f" ];
          # this one has two names?
          toggle-above = [ "<Super>s" ];
          # always-on-top = [ "<Super>s" ];
          toggle-on-all-workspaces = [ "<Super><Shift>s" ];
          activate-window-menu = [ "<Super>space" ];
          switch-input-source = [ "XF86Keyboard" ];
          switch-input-source-backward = [ "<Shift>XF86Keyboard" ];

          # no-preview window switching
          cycle-windows = [ "<Super>Tab" ];
          cycle-windows-backward = [ "<Super><Shift>Tab" ];
          # no-preview window switching within app
          cycle-group = [ "<Super>Above_Tab" ];
          cycle-group-backward = [ "<Super><Shift>Above_Tab" ];
          # disable preview (switcher)
          switch-windows = [ ];
          switch-windows-backward = [ ];
          switch-applications = [ ];
          switch-applications-backward = [ ];
          switch-group = [ ];
          switch-group-backward = [ ];

          move-to-monitor-up = [ "<Super><Shift>Up" "<Super><Shift>k" ];
          move-to-monitor-down = [ "<Super><Shift>Down" "<Super><Shift>j" ];
          move-to-monitor-left = [ "<Super><Shift>Left" "<Super><Shift>h" ];
          move-to-monitor-right = [ "<Super><Shift>Right" "<Super><Shift>l" ];
          switch-to-workspace-1 = [ "<Super>1" ];
          switch-to-workspace-2 = [ "<Super>2" ];
          switch-to-workspace-3 = [ "<Super>3" ];
          switch-to-workspace-4 = [ "<Super>4" ];
          switch-to-workspace-5 = [ "<Super>5" ];
          switch-to-workspace-6 = [ "<Super>6" ];
          switch-to-workspace-7 = [ "<Super>7" ];
          switch-to-workspace-8 = [ "<Super>8" ];
          switch-to-workspace-9 = [ "<Super>9" ];
          switch-to-workspace-10 = [ "<Super>0" ];
          move-to-workspace-1 = [ "<Super><Shift>1" ];
          move-to-workspace-2 = [ "<Super><Shift>2" ];
          move-to-workspace-3 = [ "<Super><Shift>3" ];
          move-to-workspace-4 = [ "<Super><Shift>4" ];
          move-to-workspace-5 = [ "<Super><Shift>5" ];
          move-to-workspace-6 = [ "<Super><Shift>6" ];
          move-to-workspace-7 = [ "<Super><Shift>7" ];
          move-to-workspace-8 = [ "<Super><Shift>8" ];
          move-to-workspace-9 = [ "<Super><Shift>9" ];
          move-to-workspace-10 = [ "<Super><Shift>0" ];
        };
        # folder names must follow this custom\d format, unfortunately. means I
        # can't use Nix to ensure no conflicts
        # TODO: this doesn't work on a fresh system
        "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" =
          {
            binding = "<Super>e";
            command =
              "emacsclient --create-frame --alternate-editor emacs --no-wait";
            name = "Emacsclient";
          };
        "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1" =
          {
            binding = "<Super>c";
            command = "org-capture";
            name = "org-capture";
          };
      };
    };
  };
}

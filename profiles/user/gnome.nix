{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.gnome;
in {
  options.profiles.user.gnome.enable =
    mkEnableOption "my user-level GNOME configuration";
  config = mkIf cfg.enable {
    profiles.user.wayland-base.enable = true;

    # Disable gnome-keyring ssh-agent in favor of the default agent
    xdg.configFile."autostart/gnome-keyring-ssh.desktop".text = ''
      ${lib.fileContents
      "${pkgs.gnome.gnome-keyring}/etc/xdg/autostart/gnome-keyring-ssh.desktop"}
      Hidden=true
    '';
    # TODO: add safeeyes once it works on gnome (widget issue)

    home.packages = with pkgs; [
      gnomeExtensions.sound-output-device-chooser
      gnome.dconf-editor
    ];

    # not necessary in a desktop environment
    services.network-manager-applet.enable = lib.mkForce false;

    # declaratively manage gsettings
    dconf = {
      enable = true;
      # https://rycee.gitlab.io/home-manager/options.html#opt-dconf.settings
      settings = {
        # workspaces span displays
        "org/gnome/mutter" = {
          workspaces-only-on-primary = false;
          dynamic-workspaces = false;
        };
        "org/gnome/desktop/wm/preferences".num-workspaces = 4;
        "org/gnome/desktop/interface".clock-show-weekday = true;
        "org/gnome/shell/app-switcher".current-workspace-only = true;
        "org/gnome/desktop/privacy".remove-old-trash-files = true;
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
          toggle-above = [ "<Super>s" ];
          toggle-on-all-workspaces = [ "<Super><Shift>s" ];
          activate-window-menu = [ "<Super>space" ];
          switch-input-source = [ "XF86Keyboard" ];
          switch-input-source-backward = [ "<Shift>XF86Keyboard" ];
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
      };
    };
  };
}

{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.firefox;
in {
  options.profiles.user.firefox.enable = mkEnableOption ''
    Include my Firefox config.
  '';

  config = mkIf cfg.enable {
    home.sessionVariables.BROWSER = "firefox";

    xdg = {
      # vim keybindings extension
      configFile."tridactyl/tridactylrc".source = ./tridactylrc;
      mimeApps = let
        applyToAll = list:
          builtins.listToAttrs (map (key: {
            name = key;
            value = "firefox.desktop";
          }) list);
      in {
        associations.added =
          applyToAll [ "x-scheme-handler/chrome" "application/pdf" ];
        defaultApplications = applyToAll [
          "application/xhtml+xml"
          # needed for default browser check
          "text/html"
          "x-scheme-handler/http"
          "x-scheme-handler/https"
          "x-scheme-handler/ftp"
        ];
      };
    };

    programs.firefox = {
      enable = true;
      package = pkgs.wrapFirefox pkgs.firefox-unwrapped {
        forceWayland = config.profiles.user.wayland-base.enable;
        extraPolicies = {
          DisablePocket = true;
          DontCheckDefaultBrowser = true;
          DisableSetDesktopBackground = true;
          FirefoxHome.Pocket = false;
          OfferToSaveLogins = false;
          PasswordManagerEnabled = false;
          PromptForDownloadLocation = true;
          SearchBar = "unified";
        };
        extraNativeMessagingHosts = [ pkgs.tridactyl-native ]
          ++ optional config.profiles.user.gnome.enable
          pkgs.gnomeExtensions.gsconnect;
      };
      profiles = {
        default = {
          name = "default";
          settings = {
            "general.warnOnAboutConfig" = false;
            "browser.aboutConfig.showWarning" = false;

            # TODO: have to set this for every language?
            # use sans serif rather than serif for default proportional font
            "font.default.x-western" = "sans-serif";
            # use system emoji font
            "font.name-list.emoji" = "emoji";
            # don't allow webpages to choose fonts beyond serif, sans, mono
            "browser.display.use_document_fonts" = 0;

            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

            # don't sync whether add-ons are enabled to change them per device
            "services.sync.addons.ignoreUserEnabledChanges" = true;

            # open popups in new tabs, not new windows with no UI
            "browser.link.open_newwindow.restriction" = 0;
            "browser.tabs.warnOnClose" = false;

            "browser.ctrlTab.sortByRecentlyUsed" = true;

            # shorcuts
            "browser.newtabpage.activity-stream.feeds.topsites" = false;
            "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.havePinned" =
              "";
            "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
            # recommended by pocket
            "browser.newtabpage.activity-stream.feeds.section.topstories" =
              false;
            "browser.newtabpage.activity-stream.showSponsored" = false;
            # recent activity
            "browser.newtabpage.activity-stream.feeds.section.highlights" =
              false;
            # snippets
            "browser.newtabpage.activity-stream.feeds.snippets" = false;

            # not officially supported yet on Nvidia proprietary drivers
            # but I haven't noticed bugs and it's much faster:
            # https://testdrive-archive.azurewebsites.net/Performance/Chalkboard/
            "gfx.webrender.all" = true;
            "gfx.webrender.enable" = true;
            # may improve perf?
            # "gfx.use-glx-texture-from-pixmap" = true;

            "extensions.formautofill.addresses.enabled" = false;
          };
          userChrome = builtins.readFile ./userchrome.css;
        };
        # profile for debugging
        clean = {
          name = "clean";
          id = 1;
          settings = {
            "general.warnOnAboutConfig" = false;
            "browser.aboutConfig.showWarning" = false;
            "extensions.pocket.enabled" = false;
            "extensions.formautofill.addresses.enabled" = false;
            # no history
            "browser.privatebrowsing.autostart" = true;
            "browser.newtabpage.activity-stream.feeds.topsites" = false;
            "browser.newtabpage.activity-stream.feeds.section.topstories" =
              false;
            "browser.newtabpage.activity-stream.feeds.section.highlights" =
              false;
          };
        };
      };
    };

    xsession.windowManager.i3 = {
      config = {
        window.commands = [{
          criteria = {
            class = "Firefox";
            window_role = "PictureInPicture";
          };
          command = "sticky enable; border pixel 0";
        }];
        floating.criteria = [{
          "class" = "^Firefox$";
          "window_role" = "^About$";
        }];
      };
      extraConfig = ''
        no_focus [class="Firefox" window_role="PictureInPicture"]
      '';
    };
  };
}

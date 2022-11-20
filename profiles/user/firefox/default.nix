{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.firefox;
in {
  options.profiles.user.firefox.enable = mkEnableOption ''
    Include my Firefox config.
  '';

  config = mkIf cfg.enable {
    home.sessionVariables = {
      BROWSER = "firefox";
      MOZ_DBUS_REMOTE =
        if config.profiles.user.wayland-base.enable then 1 else 0;
    };

    xdg = {
      configFile = {
        # surfingkeys requires a single-file JS config
        "surfingkeys/config.js".text = ''
          ${builtins.readFile ./surfingkeys/config.js}

          ${builtins.readFile ./surfingkeys/private.js}

          settings.theme = `
          ${builtins.readFile ./surfingkeys/theme.css}
          `
        '';
      };
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
          "x-scheme-handler/http"
          "x-scheme-handler/https"
          "text/html"
          "application/x-extension-htm"
          "application/x-extension-html"
          "application/x-extension-shtml"
          "application/xhtml+xml"
          "application/x-extension-xhtml"
          "application/x-extension-xht"
        ];
      };
    };

    # URL for surfingkeys settings: http://localhost:41066/config.js
    # NOTE: have to clear browser cache, otherwise it grabs the old version?
    systemd.user.services.surfingkeys-config = {
      Unit.Description =
        "Run a local web server to serve the surfingkeys config";
      Service.ExecStart =
        "${pkgs.miniserve}/bin/miniserve --port 41066 ${config.xdg.configHome}/surfingkeys";
      Install.WantedBy = [ "graphical-session.target" ];
    };

    programs.firefox = {
      enable = true;
      package = pkgs.wrapFirefox pkgs.firefox-unwrapped {
        forceWayland = config.profiles.user.wayland-base.enable;
        extraPolicies = {
          DisablePocket = true;
          DisableTelemetry = true;
          DontCheckDefaultBrowser = true;
          DisableSetDesktopBackground = true;
          FirefoxHome.Pocket = false;
          NewTabPage = false;
          OfferToSaveLogins = false;
          PasswordManagerEnabled = false;
          PromptForDownloadLocation = true;
          SearchBar = "unified";
          Bookmarks = optional config.services.syncthing.enable {
            Title = "Syncthing";
            URL = "http://localhost:8384";
            Favicon = "https://syncthing.net/img/favicon.png";
            Placement = "toolbar";
          } ++ optional config.profiles.user.emacs.enable {
            Title = "org-roam-ui";
            URL = "http://localhost:35901";
            Favicon =
              "https://orgmode.org/resources/img/favicons/favicon-32x32.png";
            Placement = "toolbar";
          };
        };
        extraNativeMessagingHosts = optional config.profiles.user.gnome.enable
          pkgs.gnomeExtensions.gsconnect;
      };
      profiles = {
        default = {
          name = "default";
          search = {
            force = true;
            default = "DuckDuckGo";
            order = [
              "DuckDuckGo"
              "Wikipedia (en)"
              "Google"
              "WayBack Machine"
              "archive.today"
              "WayBack Machine (all)"
              "archive.today (all)"
              "Wordnik"
              "Open Library"
              "Marginalia"
              "Discu.eu"
              "Hacker News"
              "Reddit"
              "YouTube"
              "Genius"
              "ManKier"
              "Nix Packages"
              "NixOS Options"
              "Nix Home Manager Options"
              "NixOS Wiki"
              "Reddit (URL)"
              "Nixpkgs PR Tracker"
            ];
            engines = {
              "Bing".metaData.hidden = true;
              "Amazon.com".metaData.hidden = true;
              "eBay".metaData.hidden = true;
              "Wikipedia (en)".metaData.alias = "@wk";
              "Google".metaData.alias = "@g";

              "WayBack Machine" = {
                urls =
                  [{ template = "https://web.archive.org/web/{searchTerms}"; }];
                iconUpdateURL =
                  "https://web.archive.org/_static/images/archive.ico";
                updateInterval = 24 * 60 * 60 * 1000; # daily
                definedAliases = [ "@wb" ];
              };
              "archive.today" = {
                urls = [{
                  template = "https://archive.today/newest/{searchTerms}";
                }];
                iconUpdateURL = "https://archive.today/apple-touch-icon.png";
                updateInterval = 24 * 60 * 60 * 1000;
                definedAliases = [ "@at" ];
              };
              "WayBack Machine (all)" = {
                urls = [{
                  template = "https://web.archive.org/web/*/{searchTerms}";
                }];
                iconUpdateURL =
                  "https://web.archive.org/_static/images/archive.ico";
                updateInterval = 24 * 60 * 60 * 1000;
                definedAliases = [ "@wba" ];
              };
              "archive.today (all)" = {
                urls = [{ template = "https://archive.today/{searchTerms}*"; }];
                iconUpdateURL = "https://archive.today/apple-touch-icon.png";
                updateInterval = 24 * 60 * 60 * 1000;
                definedAliases = [ "@ata" ];
              };
              "Wordnik" = {
                urls = [{
                  template = "https://wordnik.com/words/?myWord={searchTerms}";
                }];
                iconUpdateURL = "https://wordnik.com/img/favicon.png";
                updateInterval = 24 * 60 * 60 * 1000;
                definedAliases = [ "@wd" ];
              };
              "Open Library" = {
                urls = [{
                  template = "https://openlibrary.org/search?q={searchTerms}";
                }];
                iconUpdateURL =
                  "https://openlibrary.org/static/images/openlibrary-192x192.png";
                updateInterval = 24 * 60 * 60 * 1000;
                definedAliases = [ "@ol" ];
              };
              "Marginalia" = {
                urls = [{
                  template =
                    "https://search.marginalia.nu/search?query={searchTerms}";
                }];
                iconUpdateURL = "https://search.marginalia.nu/favicon.ico";
                updateInterval = 24 * 60 * 60 * 1000;
                definedAliases = [ "@mg" ];
              };
              "Discu.eu" = {
                urls = [{ template = "https://discu.eu/?q={searchTerms}"; }];
                iconUpdateURL = "https://discu.eu/static/favicon-32x32.png";
                updateInterval = 24 * 60 * 60 * 1000;
                definedAliases = [ "@du" ];
              };
              "Hacker News" = {
                urls =
                  [{ template = "https://hn.algolia.com/?q={searchTerms}"; }];
                iconUpdateURL = "https://news.ycombinator.com/favicon.ico";
                updateInterval = 24 * 60 * 60 * 1000;
                definedAliases = [ "@hn" ];
              };
              "Reddit" = {
                urls = [{
                  template = "https://old.reddit.com/search?q={searchTerms}";
                }];
                iconUpdateURL =
                  "https://www.redditstatic.com/desktop2x/img/favicon/favicon-96x96.png";
                updateInterval = 24 * 60 * 60 * 1000;
                definedAliases = [ "@rd" ];
              };
              "Reddit (URL)" = {
                urls = [{
                  template =
                    "https://old.reddit.com/search?q=url:{searchTerms}";
                }];
                iconUpdateURL =
                  "https://www.redditstatic.com/desktop2x/img/favicon/favicon-96x96.png";
                updateInterval = 24 * 60 * 60 * 1000;
                definedAliases = [ "@rdu" ];
              };
              "YouTube" = {
                urls = [{
                  template =
                    "https://www.youtube.com/results?search_query={searchTerms}";
                }];
                iconUpdateURL =
                  "https://www.youtube.com/s/desktop/271dfaff/img/favicon_144x144.png";
                updateInterval = 24 * 60 * 60 * 1000;
                definedAliases = [ "@yt" ];
              };
              "Genius" = {
                urls =
                  [{ template = "https://genius.com/search?q={searchTerms}"; }];
                iconUpdateURL = "https://genius.com/favicon.ico";
                updateInterval = 24 * 60 * 60 * 1000;
                definedAliases = [ "@gen" ];
              };
              "ManKier" = {
                urls =
                  [{ template = "https://www.mankier.com/?q={searchTerms}"; }];
                iconUpdateURL = "https://www.mankier.com/favicon.ico";
                updateInterval = 24 * 60 * 60 * 1000;
                definedAliases = [ "@man" ];
              };
              "Nix Packages" = {
                urls = [{
                  template =
                    "https://search.nixos.org/packages?channel=unstable&query={searchTerms}";
                }];
                icon =
                  "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@nxp" ];
              };
              "Nixpkgs PR Tracker" = {
                urls = [{
                  template =
                    "https://nixpk.gs/pr-tracker.html?pr={searchTerms}";
                }];
                icon =
                  "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@nxpr" ];
              };
              "NixOS Options" = {
                urls = [{
                  template =
                    "https://search.nixos.org/options?channel=unstable&query={searchTerms}";
                }];
                icon =
                  "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@nxo" ];
              };
              "Nix Home Manager Options" = {
                urls = [{
                  template =
                    "https://mipmip.github.io/home-manager-option-search/?{searchTerms}";
                }];
                icon =
                  "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@nxh" ];
              };
              "NixOS Wiki" = {
                urls = [{
                  template =
                    "https://nixos.wiki/index.php?search={searchTerms}";
                }];
                iconUpdateURL = "https://nixos.wiki/favicon.png";
                updateInterval = 24 * 60 * 60 * 1000;
                definedAliases = [ "@nxw" ];
              };
            };
          };
          settings = {
            "general.warnOnAboutConfig" = false;
            "browser.aboutConfig.showWarning" = false;
            "browser.shell.checkDefaultBrowser" = false;
            # recommended add-ons
            "extensions.getAddons.showPane" = false;
            "extensions.htmlaboutaddons.recommendations.enabled" = false;
            "browser.discovery.enabled" = false;

            # restore previous session
            "browser.startup.page" = 3;
            # clear third-party cookies on close
            "network.cookie.thirdparty.sessionOnly" = true;
            "network.cookie.thirdparty.nonsecureSessionOnly" = true;

            # containers
            "privacy.userContext.enabled" = true;
            "privacy.userContext.ui.enabled" = true;

            "browser.contentblocking.category" = "strict";

            # accept more than 3 fontconfig fallbacks for generic families
            "gfx.font_rendering.fontconfig.max_generic_substitutions" = 255;

            # use sans serif rather than serif for default proportional font
            "font.default.ar" = "sans-serif";
            "font.default.el" = "sans-serif";
            "font.default.he" = "sans-serif";
            "font.default.ja" = "sans-serif";
            "font.default.ko" = "sans-serif";
            "font.default.th" = "sans-serif";
            "font.default.x-armn" = "sans-serif";
            "font.default.x-beng" = "sans-serif";
            "font.default.x-cans" = "sans-serif";
            "font.default.x-cyrillic" = "sans-serif";
            "font.default.x-devanagari" = "sans-serif";
            "font.default.x-ethi" = "sans-serif";
            "font.default.x-geor" = "sans-serif";
            "font.default.x-gujr" = "sans-serif";
            "font.default.x-guru" = "sans-serif";
            "font.default.x-khmr" = "sans-serif";
            "font.default.x-knda" = "sans-serif";
            "font.default.x-math" = "sans-serif";
            "font.default.x-mlym" = "sans-serif";
            "font.default.x-orya" = "sans-serif";
            "font.default.x-sinh" = "sans-serif";
            "font.default.x-tamil" = "sans-serif";
            "font.default.x-telu" = "sans-serif";
            "font.default.x-tibt" = "sans-serif";
            "font.default.x-unicode" = "sans-serif";
            "font.default.x-western" = "sans-serif";
            "font.default.zh-CN" = "sans-serif";
            "font.default.zh-HK" = "sans-serif";
            "font.default.zh-TW" = "sans-serif";

            # use system emoji font
            "font.name-list.emoji" = "emoji";
            # firefox uses a specific font list here for some reason
            "font.name-list.serif.x-math" = "serif";

            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

            # don't sync whether add-ons are enabled to change them per device
            "services.sync.addons.ignoreUserEnabledChanges" = true;

            # open popups in new tabs, not new windows with no UI
            "browser.link.open_newwindow.restriction" = 0;
            "browser.tabs.warnOnClose" = false;

            # F11 hides chrome while remaining windowed
            # true fullscreen still available via window manager binding
            "full-screen-api.ignore-widgets" = true;

            "browser.ctrlTab.sortByRecentlyUsed" = true;

            # pipewire and gnome shell handle this
            "privacy.webrtc.legacyGlobalIndicator" = false;
            "privacy.webrtc.hideGlobalIndicator" = true;

            # blank page allows surfingkeys
            # windows and homepage
            "browser.startup" = "about.blank";

            # shortcuts
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

            "browser.tabs.firefox-view" = false;

            "extensions.formautofill.addresses.enabled" = false;
            "extensions.formautofill.creditCards.available" = false;
            "extensions.formautofill.creditCards.enabled" = false;

            # a malicious extension with perms for all sites can already steal everything non-mozilla
            "extensions.webextensions.restrictedDomains" = "";
            "privacy.resistFingerprinting.block_mozAddonManager" = true;
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

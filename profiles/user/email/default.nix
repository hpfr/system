{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.email;
in {
  options.profiles.user.email.enable = mkEnableOption "my email configuration";
  config = mkIf cfg.enable {
    accounts.email = {
      maildirBasePath = "documents/maildirs";
      accounts = {
        hpfr = {
          address = "liam@hpfr.net";
          userName = "liam@hpfr.net";
          realName = "Liam Hupfer";
          primary = true;
          imap.host = "imap.migadu.com";
          smtp = {
            host = "smtp.migadu.com";
            port = 465;
            tls.useStartTls = false;
          };
          passwordCommand =
            "${pkgs.libsecret}/bin/secret-tool lookup Uuid 80f91fdfe9a545b7a0ee325c0d8b2042";
          folders = {
            inbox = "INBOX";
            sent = "Sent";
            trash = "Trash";
          };
          imapnotify = {
            enable = true;
            boxes = [ "INBOX" "Archive" "Drafts" "Junk" "Sent" "Trash" ];
            onNotify = let
              onNotifyScript = pkgs.writeShellScript "hpfr-notify.sh" ''
                set -euo pipefail
                mailbox="$1"

                case "$mailbox" in
                  "INBOX") ${pkgs.isync}/bin/mbsync --pull hpfr-inbox;;
                  *) ${pkgs.isync}/bin/mbsync --pull hpfr-unchanged:"$mailbox";;
                esac
              '';
            in "${onNotifyScript} '%s'";
            onNotifyPost = let
              onNotifyPostScript =
                pkgs.writeShellScript "hpfr-notify-post.sh" ''
                  set -euo pipefail
                  mailbox="$1"
                  if ${pkgs.mu}/bin/mu index --lazy-check; then
                    test -f /tmp/mu_reindex_now && rm /tmp/mu_reindex_now
                    if [ "$mailbox" == "INBOX" ]; then
                      ${pkgs.libnotify}/bin/notify-send "hpfr: You've got mail"
                    fi
                  else
                    touch /tmp/mu_reindex_now
                  fi
                '';
            in "${onNotifyPostScript} '%s'";
          };
          mbsync = {
            enable = true;
            create = "maildir";
            expunge = "both";
            extraConfig.channel.CopyArrivalDate = "yes";
            groups.hpfr.channels = {
              unchanged = {
                patterns = [
                  "%"
                  # renames
                  "!INBOX"
                  "!Inbox"
                ];
                extraConfig = {
                  Create = "Near";
                  Expunge = "both";
                  CopyArrivalDate = "yes";
                  SyncState = "*";
                };
              };
              inbox = {
                farPattern = "INBOX";
                nearPattern = "Inbox";
                extraConfig = {
                  Create = "Near";
                  Expunge = "both";
                  CopyArrivalDate = "yes";
                  SyncState = "*";
                };
              };
            };
          };
          msmtp.enable = true;
        };
      } // import ./private.nix { inherit pkgs; };
    };

    programs = {
      mu.enable = true;
      msmtp.enable = true;
      mbsync.enable = true;
    };

    services.imapnotify.enable = true;
  };
}

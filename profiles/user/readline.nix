{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.readline;
in {
  options.profiles.user.readline.enable = mkEnableOption "my readline configuration";
  config = mkIf cfg.enable {
    # vi mode for bash, other shells like python
    programs.readline = {
      enable = true;
      variables = {
        show-mode-in-prompt = true;
        editing-mode = "vi";
      };
      bindings = {
        "\\e[A" = "history-search-backward";
        "\\e[B" = "history-search-forward";
      };
      extraConfig = ''
        $if term=linux
          set vi-ins-mode-string \1\e[?0c\2
          set vi-cmd-mode-string \1\e[?8c\2
        $else
          set vi-ins-mode-string \1\e[6 q\2
          set vi-cmd-mode-string \1\e[2 q\2
        $endif
        set keymap vi-command
        # these are for vi-command mode
        j: history-search-forward
        k: history-search-backward
      '';
    };
  };
}

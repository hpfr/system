{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.fish;
in {
  options.profiles.user.fish.enable = mkEnableOption "my fish configuration";

  config = mkIf cfg.enable {
    programs = {
      fish = {
        enable = true;
        interactiveShellInit = ''
          # fish-foreign-env does not handle $()
          set LESS_TERMCAP_mb (tput bold; tput setaf 2)
          set LESS_TERMCAP_md (tput bold; tput setaf 6)
          set LESS_TERMCAP_me (tput sgr0)
          set LESS_TERMCAP_so (tput bold; tput setaf 3; tput setab 4)
          set LESS_TERMCAP_se (tput rmso; tput sgr0)
          set LESS_TERMCAP_us (tput smul; tput bold; tput setaf 7)
          set LESS_TERMCAP_ue (tput rmul; tput sgr0)
          set LESS_TERMCAP_mr (tput rev)
          set LESS_TERMCAP_mh (tput dim)
          set LESS_TERMCAP_ZN (tput ssubm)
          set LESS_TERMCAP_ZV (tput rsubm)
          set LESS_TERMCAP_ZO (tput ssupm)
          set LESS_TERMCAP_ZW (tput rsupm)

          fish_vi_key_bindings
          # Emulates vim's cursor shape behavior
          # Set the normal and visual mode cursors to a block
          set fish_cursor_default block
          # Set the insert mode cursor to a line
          set fish_cursor_insert line
          # Set the replace mode cursor to an underscore
          set fish_cursor_replace_one underscore
          # The following variable can be used to configure cursor shape in
          # visual mode, but due to fish_cursor_default, is redundant here
          set fish_cursor_visual block

          # NixOS fish config makes unwanted ls alias that takes precedence over
          # ls function in ~/.config/fish/functions
          function ls
            ${config.programs.fish.functions.ls.body}
          end
        '';
        shellAbbrs = {
          mkd = "mkdir -pv";
          nrs = "sudo nixos-rebuild switch";
          nrsl = "sudo nixos-rebuild switch -option builders ''";
          nrsu = "sudo nix-channel --update; sudo nixos-rebuild switch";
          sctl = "sudo systemctl";
          uctl = "systemctl --user";
        };
        functions = {
          ls.body = ''
            command ls --human-readable --literal --color=auto \
            --group-directories-first $argv
          '';
          grep.body = "command grep --color=auto $argv";
          diff.body = "command diff --color $argv";
          pgrep.body = "command pgrep --list-name --ignore-case $argv";
          # remove welcome message
          fish_greeting = "";
        };
      };

      starship.enableFishIntegration = true;

      alacritty.settings.shell.program = "fish";
      foot.settings.main.shell = "fish";
    };
  };
}

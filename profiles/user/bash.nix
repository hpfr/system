{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.bash;
in {
  options.profiles.user.bash.enable = mkEnableOption "my bash configuration";

  config = mkIf cfg.enable {
    programs.bash = {
      enable = true;
      shellOptions = [
        # Append to history file rather than replacing
        "histappend"
        # extended globbing
        "extglob"
        "globstar"
        # warn if closing shell with running jobs
        "checkjobs"
        # cd by typing directory name alone
        "autocd"
      ];
      shellAliases = {
        mkd = "mkdir -pv";
        nrs = "doas nixos-rebuild switch";
        nrsl = "doas nixos-rebuild switch --option builders ''";
        nrsu = "doas nix-channel --update; doas nixos-rebuild switch";
        ls = "ls -hN --color=auto --group-directories-first";
        grep = "grep --color=auto";
        diff = "diff --color";
        yt =
          "youtube-dl --add-metadata -i -o '%(upload_date)s-%(title)s.%(ext)s'";
        yta = "yt -x -f bestaudio/best";
        ffmpeg = "ffmpeg -hide_banner";
        ffplay = "ffplay -hide_banner";
        ffprobe = "ffprobe -hide_banner";
      };
      # keep tput in interactive commands, otherwise it can break things using
      # non-interactive shells like remote builds
      initExtra = ''
        stty -ixon # disable ctrl-s and ctrl-q
        # https://wiki.archlinux.org/index.php/Bash/Prompt_customization
        export PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\W\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"

        # less colors
        # https://unix.stackexchange.com/questions/119/colors-in-man-pages/147#147
        # https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
        export LESS_TERMCAP_mb="$(tput bold; tput setaf 2)" # green
        export LESS_TERMCAP_md="$(tput bold; tput setaf 6)" # cyan
        export LESS_TERMCAP_me="$(tput sgr0)"
        export LESS_TERMCAP_so="$(tput bold; tput setaf 3; tput setab 4)" # yellow on blue
        export LESS_TERMCAP_se="$(tput rmso; tput sgr0)"
        export LESS_TERMCAP_us="$(tput smul; tput bold; tput setaf 7)" # white
        export LESS_TERMCAP_ue="$(tput rmul; tput sgr0)"
        export LESS_TERMCAP_mr="$(tput rev)"
        export LESS_TERMCAP_mh="$(tput dim)"
        export LESS_TERMCAP_ZN="$(tput ssubm)"
        export LESS_TERMCAP_ZV="$(tput rsubm)"
        export LESS_TERMCAP_ZO="$(tput ssupm)"
        export LESS_TERMCAP_ZW="$(tput rsupm)"
      '';
    };
  };
}

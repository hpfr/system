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
        nrs = "sudo nixos-rebuild switch";
        nrsl = "sudo nixos-rebuild switch -option builders ''";
        nrsu = "sudo nix-channel --update; sudo nixos-rebuild switch";
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
      initExtra = ''
        stty -ixon # disable ctrl-s and ctrl-q
        # https://wiki.archlinux.org/index.php/Bash/Prompt_customization
        export PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\W\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"
      '';
    };
  };
}

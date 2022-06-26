{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.mpv;
in {
  options.profiles.user.mpv.enable = mkEnableOption "my mpv configuration";

  config = mkIf cfg.enable {
    programs.mpv = {
      enable = true;
      scripts = with pkgs.mpvScripts; [
        mpris
        mpv-playlistmanager
        youtube-quality
      ];
      config = {
        # prefer 1080p for faster download
        ytdl-format =
          "bestvideo[height<=1080]+bestaudio/best[height<=1080]/bestvideo*+bestaudio/best";
        # include subtitles
        ytdl-raw-options = ''sub-lang="en",write-sub=,write-auto-sub='';
      };
      bindings = {
        h = "seek -5";
        j = "seek -60";
        k = "seek 60";
        l = "seek 5";

        # rebind lost l binding, matches across from L which loops whole file
        H = "ab-loop";
        # rebind lost j binding, move J to K
        J = "cycle sub";
        K = "cycle sub down";
      };
    };
  };
}

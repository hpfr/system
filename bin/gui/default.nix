{ stdenv, lib, resholvePackage, bashInteractive, doCheck ? true, shellcheck,

coreutils, xwallpaper, glib, swaybg, rofi, xdotool, gnused, gawk, gnugrep
, findutils, xclip, libnotify, xorg, libxml2, curl, maim, xrandr, arandr
, i3-gaps,

withX ? false, withWayland ? false, withGnome ? false }:

resholvePackage rec {
  pname = "gui-scripts";
  version = "unreleased";

  src = ./.;

  installPhase = ''
    shopt -s globstar
    runHook preInstall
    for tool in ${./.}"/"**; do
      [ -f $tool ] && install -D -m755 $tool $out/bin/$(basename $tool)
    done
    runHook postInstall
  '';

  solutions = {
    # Give each solution a short name. This is what you'd use to
    # override its settings, and it shows in (some) error messages.
    profile = {
      # the only *required* arguments are the 3 below

      # Specify 1 or more $out-relative script paths. Unlike many
      # builders, resholvePackage modifies the output files during
      # fixup (to correctly resolve in-package sourcing).
      scripts = [ "setbg" ] ++ lib.optionals withX [
        "xcmds/displayselect"
        "xcmds/hover"
        "xcmds/maimpick"
        "xcmds/rofi-emoji"
        "xcmds/showclip"
        "xcmds/winresize"
        "xcmds/i3cmds/i3resize"
        "xcmds/i3cmds/i3wininfo"
      ];

      # "none" for no shebang, "${bash}/bin/bash" for bash, etc.
      interpreter = "${bashInteractive}/bin/bash";

      # packages resholve should resolve executables from
      inputs = [ coreutils ] ++ lib.optionals withX [
        xwallpaper
        rofi
        xorg.xwininfo # query window information
        xorg.xprop # query window properties
        xorg.xdpyinfo # get info like DPI
        xdotool # manage windows
        gnused
        gawk
        gnugrep
        findutils
        xclip # clipboard
        libnotify
        libxml2
        curl
        maim
        xrandr
        arandr
        i3-gaps
      ] ++ lib.optionals withWayland [ swaybg ]
        ++ lib.optionals withGnome [ glib ];
    };
  };

  makeFlags = [ "prefix=${placeholder "out"}" ];

  inherit doCheck;
  checkInputs = [ shellcheck ];

  # ...
}

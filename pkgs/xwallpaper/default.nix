{ stdenv, fetchFromGitHub, libjpeg, libpng, libseccomp, libXpm, pixman, xcbutil
, xcbutilimage, pkgconfig, autoreconfHook }:

stdenv.mkDerivation rec {
  pname = "xwallpaper";
  version = "0.6.4";

  src = fetchFromGitHub {
    owner = "stoeckmann";
    repo = "xwallpaper";
    rev = "v" + version;
    sha256 = "175fzifvia58vah2x7509drvfn3xfv5d9szgh9x1w1a1w8rcs2hx";
  };

  buildInputs = [
    libjpeg
    libpng
    # libseccomp
    libXpm
    pixman
    xcbutil
    xcbutilimage
    pkgconfig
    autoreconfHook
  ];
}

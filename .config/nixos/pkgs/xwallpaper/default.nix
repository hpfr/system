{ stdenv, fetchFromGitHub, libjpeg, libpng, libseccomp, libXpm, pixman, xcbutil
, xcbutilimage, pkgconfig, autoreconfHook }:

stdenv.mkDerivation rec {
  pname = "xwallpaper";
  version = "0.6.2";

  src = fetchFromGitHub {
    owner = "stoeckmann";
    repo = "xwallpaper";
    rev = "v" + version;
    sha256 = "1cbb7czkpwkw1g9n2y6y3jsb01s8qwsip2li0jqjj4m5xy8hj40r";
  };

  buildInputs = [
    libjpeg
    libpng
    libseccomp
    libXpm
    pixman
    xcbutil
    xcbutilimage
    pkgconfig
    autoreconfHook
  ];
}

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
    libseccomp
    libXpm
    pixman
    xcbutil
    xcbutilimage
    pkgconfig
    autoreconfHook
  ];

  meta = with stdenv.lib; {
    description = "A simple wallpaper utility for X";
    longDescription = ''
      The xwallpaper utility allows you to set image files as your X wallpaper.
      JPEG, PNG, and XPM file formats are supported, all of them being
      configurable and therefore no fixed dependencies.

      The wallpaper is also advertised to programs which support
      semi-transparent backgrounds.
    '';
    homepage = "https://github.com/stoeckmann/xwallpaper";
    license = licenses.isc;
    # maintainers = with maintainers; [ liam ];
    platforms = platforms.unix;
  };
}

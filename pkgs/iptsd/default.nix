{ stdenv, fetchFromGitHub, meson, ninja, pkg-config, systemd, inih }:

stdenv.mkDerivation rec {
  pname = "iptsd";
  version = "0.3.1";
  src = fetchFromGitHub {
    owner = "linux-surface";
    repo = pname;
    rev = "v" + version;
    sha256 = "1007id9lijrpj69yc5wwip2gk3rbvnfjjwxj92xmniww5nfvdjsf";
  };

  nativeBuildInputs = [ meson ninja pkg-config ];

  buildInputs = [ systemd inih ];

  mesonFlags = [
    # should be able to enable the systemd flag with the right nixos options
    # like systemd.packages and services.udev.packages, but the build failed
    # when I used those and dropped the disable
    "-Dsystemd=false"

    "-Dsample_config=false"
    "-Ddebug_tool=false"
  ];

  meta = with stdenv.lib; {
    description = "Userspace daemon for Intel Precise Touch & Stylus";
    homepage = "https://github.com/linux-surface/iptsd";
    license = licenses.gpl2Only;
    maintainers = with maintainers; [ ];
    platforms = platforms.linux;
  };
}

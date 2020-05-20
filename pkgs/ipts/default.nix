{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  pname = "ipts";
  version = "1.0.0-2";

  src = fetchFromGitHub {
    owner = "linux-surface";
    repo = "surface-ipts-firmware";
    rev = "v" + version;
    sha256 = "15mp23m58f4jl6v4whaxb2921ywzccsw4gbypb0y5xay23w746qn";
  };

  installPhase = ''
    mkdir -p $out/lib/firmware/intel/ipts/
    cp -r $src/firmware/intel/ipts/* $out/lib/firmware/intel/ipts/
  '';
}

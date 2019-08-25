{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "ipts";

  src = ./src;

  installPhase = ''
    mkdir -p $out/lib/firmware/intel/ipts/
    cp $src/* $out/lib/firmware/intel/ipts/
  '';
}

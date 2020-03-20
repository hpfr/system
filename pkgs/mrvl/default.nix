{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "mrvl";

  src = ./src;

  installPhase = ''
    mkdir -p $out/lib/firmware/mrvl/
    cp $src/* $out/lib/firmware/mrvl/
  '';
}

{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "mwlwifi";

  src = ./src;

  installPhase = ''
    mkdir -p $out/lib/firmware/mwlwifi/
    cp $src/* $out/lib/firmware/mwlwifi/
  '';
}

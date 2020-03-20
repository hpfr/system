{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "i915";

  src = ./src;

  installPhase = ''
    mkdir -p $out/lib/firmware/i915/
    cp $src/* $out/lib/firmware/i915/
  '';
}

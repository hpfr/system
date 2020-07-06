# TODO: nixfmt maybe shouldn't split on commas?
{ stdenv, lib, buildPythonApplication, fetchPypi, fetchFromGitHub, ecdsa, hidapi
, aenum, six, prompt_toolkit, ed25519, cython }:

buildPythonApplication rec {
  pname = "python-onlykey";
  version = "1.1.0";

  src = fetchFromGitHub {
    owner = "trustcrypto";
    repo = "python-onlykey";
    rev = "61afe24d2eba3eae6e41fabbc4eca638f7a6e374";
    sha256 = "1wfnmm0fdd0nbb7hw9b9z682gqk0svngxmzqvmnjyx5in5y8d1a4";
  };

  # doCheck = false;

  propagatedBuildInputs =
    [ ecdsa hidapi aenum six prompt_toolkit ed25519 cython ];

  meta = with lib; {
    homepage = "https://github.com/trustcrypto/${pname}";
    description = "OnlyKey management CLI";
  };
}

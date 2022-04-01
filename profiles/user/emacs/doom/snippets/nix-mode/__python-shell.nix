# -*- mode: snippet -*-
# name: Python Nix shell template
# group: file templates
# --
{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    ${0:`%`}
  ];
}

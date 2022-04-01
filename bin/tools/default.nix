{ stdenv, lib, resholvePackage, bashInteractive, doCheck ? true, shellcheck,

coreutils, gnused, gawk, gnugrep, util-linux, fio }:

resholvePackage rec {
  pname = "tool-scripts";
  version = "unreleased";

  src = ./.;

  solutions = {
    # Give each solution a short name. This is what you'd use to
    # override its settings, and it shows in (some) error messages.
    profile = {
      # the only *required* arguments are the 3 below

      # Specify 1 or more $out-relative script paths. Unlike many
      # builders, resholvePackage modifies the output files during
      # fixup (to correctly resolve in-package sourcing).
      scripts = [ "diskmark" ];

      # "none" for no shebang, "${bash}/bin/bash" for bash, etc.
      interpreter = "${bashInteractive}/bin/bash";

      # packages resholve should resolve executables from
      inputs = [ coreutils gnused gawk gnugrep util-linux fio ];
    };
  };

  makeFlags = [ "prefix=${placeholder "out"}" ];

  inherit doCheck;
  checkInputs = [ shellcheck ];

  # ...
}

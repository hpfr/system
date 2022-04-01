driverPackage:

let
  # https://github.com/keylase/nvidia-patch/blob/master/patch-fbc.sh
  # https://github.com/babbaj/nix-config/blob/master/nvfbc-unlock.nix
  patches = {
    "470.42.01" =
      "s/\\x83\\xfe\\x01\\x73\\x08\\x48/\\x83\\xfe\\x00\\x72\\x08\\x48/";
    "470.57.02" =
      "s/\\x83\\xfe\\x01\\x73\\x08\\x48/\\x83\\xfe\\x00\\x72\\x08\\x48/";
    "470.63.01" =
      "s/\\x83\\xfe\\x01\\x73\\x08\\x48/\\x83\\xfe\\x00\\x72\\x08\\x48/";
    "470.74" =
      "s/\\x83\\xfe\\x01\\x73\\x08\\x48/\\x83\\xfe\\x00\\x72\\x08\\x48/";
    "470.94" =
      "s/\\x83\\xfe\\x01\\x73\\x08\\x48/\\x83\\xfe\\x00\\x72\\x08\\x48/";
    "495.44" =
      "s/\\x83\\xfe\\x01\\x73\\x08\\x48/\\x83\\xfe\\x00\\x72\\x08\\x48/";
    "495.46" =
      "s/\\x83\\xfe\\x01\\x73\\x08\\x48/\\x83\\xfe\\x00\\x72\\x08\\x48/";
    "510.47.03" =
      "s/\\x83\\xfe\\x01\\x73\\x08\\x48/\\x83\\xfe\\x00\\x72\\x08\\x48/";
  };
in driverPackage.overrideAttrs ({ version, preFixup ? "", ... }: {
  preFixup = preFixup + ''
    sed -i '${patches.${version}}' $out/lib/libnvidia-fbc.so.${version}
  '';
})

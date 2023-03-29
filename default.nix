{ pkgs ? (import <darwin> {}).pkgs

# , rev    ? "28c2c0156da98dbe553490f904da92ed436df134"
# , sha256 ? "04f3qqjs5kd5pjmqxrngjrr72lly5azcr7njx71nv1942yq1vy2f"
# , pkgs   ? import (builtins.fetchTarball {
#     url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
#     inherit sha256; }) {
#     config.allowUnfree = true;
#   }

, mkDerivation ? null
, yuicompressor  ? pkgs.yuicompressor
, sitebuilder ? pkgs.callPackage ~/src/sitebuilder { inherit pkgs yuicompressor; }
}:

with pkgs; stdenv.mkDerivation {
  name = "newartisans";
  src = ./.;
  buildInputs = [ yuicompressor sitebuilder ];
  buildPhase = "sitebuilder rebuild";
  installPhase = ''
    mkdir -p $out/share/html
    cp -pR _site $out/share/html/newartisans
  '';
}

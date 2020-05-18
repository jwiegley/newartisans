{ rev    ? "28c2c0156da98dbe553490f904da92ed436df134"
, sha256 ? "04f3qqjs5kd5pjmqxrngjrr72lly5azcr7njx71nv1942yq1vy2f"
, pkgs   ?
  import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = true;
    config.packageOverrides = pkgs: rec {
      sitebuilder = pkgs.callPackage ~/src/sitebuilder {
        compiler = "ghc883";
      };
    };
  }

, mkDerivation ? null
}:

with pkgs; stdenv.mkDerivation {
  name = "newartisans";
  src = ./.;

  buildInputs = [ yuicompressor ];

  buildPhase = "${pkgs.sitebuilder}/bin/sitebuilder rebuild";

  installPhase = ''
    mkdir -p $out/share/html
    DESTDIR=$out/share/html/newartisans
    cp -pR _site $DESTDIR

    mkdir -p $out/bin
    cat <<EOF > $out/bin/publish-newartisans
#!${pkgs.bash}/bin/bash
${pkgs.lftp}/bin/lftp \
  -u johnw@newartisans.com,\$(${pkgs.pass}/bin/pass show ftp.fastmail.com | ${pkgs.coreutils}/bin/head -1) \
  ftp://johnw@newartisans.com@ftp.fastmail.com \
  -e "set ftp:ssl-allow no; mirror --reverse $DESTDIR /johnw.newartisans.com/files/newartisans ; quit"
EOF
    chmod +x $out/bin/publish-newartisans
  '';
}

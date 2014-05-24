{ cabal, hakyll, pandoc, pandocTypes, blazeHtml, blazeMarkup }:

cabal.mkDerivation (self: {
  pname = "newartisans";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ hakyll pandoc pandocTypes blazeHtml blazeMarkup ];
  meta = {
    homepage = "http://newartisans.com";
    description = "Lost in Technopolis";
    license = self.stdenv.lib.licenses.bsd3;
  };
})

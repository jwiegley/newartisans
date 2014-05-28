{ cabal, ghc, hakyll, pandoc, pandocTypes, blazeHtml, blazeMarkup
, conduit, conduitCombinators, conduitExtra, processConduit
, yuicompressor, text, split, strict, temporary, parsec
, systemFilepath, attoparsec
}:

cabal.mkDerivation (self: {
  pname = "newartisans";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    hakyll pandoc pandocTypes blazeHtml blazeMarkup text conduit
    conduitCombinators conduitExtra processConduit yuicompressor
    split strict temporary parsec systemFilepath attoparsec
  ];
  propagatedBuildInputs = [ ghc ];
  meta = {
    homepage = "http://newartisans.com";
    description = "Lost in Technopolis";
    license = self.stdenv.lib.licenses.bsd3;
  };
})

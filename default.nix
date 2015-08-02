{ mkDerivation, attoparsec, base, binary, blaze-html, blaze-markup
, directory, exceptions, filepath, foldl, hakyll, lens, old-locale
, pandoc, pandoc-types, parsec, pipes, pipes-attoparsec
, pipes-bytestring, pipes-group, pipes-safe, pipes-shell
, pipes-text, process, split, stdenv, strict, temporary, text, time
, transformers, yuicompressor
}:
mkDerivation {
  pname = "newartisans";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    attoparsec base binary blaze-html blaze-markup directory exceptions
    filepath foldl hakyll lens old-locale pandoc pandoc-types parsec
    pipes pipes-attoparsec pipes-bytestring pipes-group pipes-safe
    pipes-shell pipes-text process split strict temporary text time
    transformers yuicompressor
  ];
  homepage = "http://newartisans.com/";
  description = "Lost in Technopolis";
  license = stdenv.lib.licenses.unfree;
}

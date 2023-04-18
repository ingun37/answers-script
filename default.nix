{ mkDerivation, aeson, aeson-pretty, base, base16-bytestring
, bytestring, containers, cryptohash-sha1, directory
, directory-tree, filepath, hlibgit2, hspec, lens, lib
, optparse-applicative, pandoc, pcre-heavy, pcre-light, QuickCheck
, text
}:
mkDerivation {
  pname = "answers-script";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base base16-bytestring bytestring containers
    cryptohash-sha1 directory directory-tree filepath hlibgit2 lens
    pandoc pcre-heavy pcre-light text
  ];
  executableHaskellDepends = [ base optparse-applicative ];
  testHaskellDepends = [
    base bytestring directory directory-tree filepath hspec QuickCheck
  ];
  doCheck = false;
  license = "unknown";
  mainProgram = "answers-script";
}

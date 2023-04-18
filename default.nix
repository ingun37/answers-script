{ mkDerivation, aeson, aeson-pretty, base, base16-bytestring
, bytestring, containers, cryptohash-sha1, directory
, directory-tree, filepath, hlibgit2, lens, lib
, optparse-applicative, pandoc, pcre-heavy, pcre-light, text
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
  license = "unknown";
  mainProgram = "answers-script";
}

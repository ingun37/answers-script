with (import <nixpkgs> {});
haskell.lib.buildStackProject {
  name = "answers-script";
  src = ./.;
  buildInputs = [pcre];
}
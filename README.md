# Answers Script

## Generate default.nix

```shell
nix-shell -p cabal2nix
cabal2nix ./. > default.nix
```

## Generate shell.nix

```shell
nix-shell -p cabal2nix
cabal2nix --shell ./. > shell.nix
```

## Build

**Don't just build using Cabal!!** it will take forever because of Pandoc.

Build in Nix environment

```shell
# Enter Nix environment defined in shell.nix
nix-shell
# Use executable
answers-script ...
```

## Install from other machines

```shell
TAR="https://github.com/ingun37/answers-script/archive/refs/tags/test2.tar.gz"
nix-shell -p "with import <nixpkgs> {}; let f = import (fetchTarball $TAR); in haskellPackages.callPackage f {}"
```
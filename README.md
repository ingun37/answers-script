# Answers Script

## Generate default.nix

```shell
nix-shell -p cabal2nix
cabal2nix --no-check ./. > default.nix
```

## Generate shell.nix

```shell
nix-shell -p cabal2nix
cabal2nix --shell --no-check ./. > shell.nix
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

## Test

```shell
nix-shell
cabal --enable-nix test
```

## Install from other machines

```shell
TAR="https://github.com/ingun37/answers-script/archive/refs/tags/test2.tar.gz"
# sandboxing
nix-shell -p "with import <nixpkgs> {}; let f = import (fetchTarball $TAR); in haskellPackages.callPackage f {}"
# no sandboxing
nix-env --install -E "with import <nixpkgs> {}; let f = import (fetchTarball $TAR); in _: (haskellPackages.callPackage f {})"
```
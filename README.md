# Build MacOS

```sh
# for building gitlib packages 
brew install pkgconf
brew install icu4c
brew install openssl@3

# /usr/local/opt/openssl is hard coded in the gitlib build setting.
ln -s $(brew --prefix openssl@3)/3.5.2 /usr/local/opt/openssl

echo "export PKG_CONFIG_PATH=\"$(brew --prefix)/opt/icu4c/lib/pkgconfig\"" >> ~/.zprofile
```

```sh
cabal build
```

# Data flow

```mermaid
flowchart LR
    n1["src"] --> n2(("directory-tree<br>readDirectoryWithL"))
    n3["myReader"] --> n2
    n2 --> n4["AnchoredDirTree FileType"]
    n4 --> n5(("directory-tree<br>filterDir"))
    n5 --> n8(("unfoldTree"))
    n9["myUnfolder"] --> n8
    n8 --> n10["Tree Item"]
    n11["myFilter"] --> n5
    n10 --> n12(("recurse"))
    n13["myWriter"] --> n12
    n12 --> n14["[Effect]"]

    n1@{ shape: rect}
    n9@{ shape: rect}
```

# CI/CD

Check the sha1 of gitlib like this

```sh
nix-shell -p nix-prefetch-git 
nix-prefetch-git https://github.com/jwiegley/gitlib.git bf256617179d853bdbc12e9283b3f570ebb9d9d7 --fetch-submodules
```

Output is like

```
git revision is bf256617179d853bdbc12e9283b3f570ebb9d9d7
path is /nix/store/63bx4k5nwjqwk7gv0a0k8adq796bjbpr-gitlib-bf25661
git human-readable version is -- none --
Commit date is 2025-09-04 11:17:27 -0700
hash is 13k3aymqwzpcijnjjka820nv6rkgakzbvh13glw98p1c4yhqwcbf
{
  "url": "https://github.com/jwiegley/gitlib.git",
  "rev": "bf256617179d853bdbc12e9283b3f570ebb9d9d7",
  "date": "2025-09-04T11:17:27-07:00",
  "path": "/nix/store/63bx4k5nwjqwk7gv0a0k8adq796bjbpr-gitlib-bf25661",
  "sha256": "13k3aymqwzpcijnjjka820nv6rkgakzbvh13glw98p1c4yhqwcbf",
  "hash": "sha256-bjGOoScsXJQ4fSPAvf5Ub2azLRBITSmtjOx+jqtXY44=",
  "fetchLFS": false,
  "fetchSubmodules": true,
  "deepClone": false,
  "fetchTags": false,
  "leaveDotGit": false,
  "rootDir": ""
}
```

Use the "sha256" for the `--sha256` field in the `cabal.project`.
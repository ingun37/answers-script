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
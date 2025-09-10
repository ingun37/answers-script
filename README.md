# Build MacOS

```sh
# for building gitlib packages 
brew install icu4c
brew install openssl@3

# /usr/local/opt/openssl is hard coded in the gitlib build setting
ln -s /opt/homebrew/Cellar/openssl@3/3.5.2 /usr/local/opt/openssl
```

then add

```sh
export PKG_CONFIG_PATH="/opt/homebrew/opt/icu4c@77/lib/pkgconfig:${PKG_CONFIG_PATH}"
```

to `.zprofile`

```sh
cabal build
```
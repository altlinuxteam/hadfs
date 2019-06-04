# hadfs

# prepare, build and run
```sh
git clone https://github.com/omgbebebe/hadfs.git
cd hadfs
git submodule update --init --recursive
cabal new-build
mkdir ./mnt
cabal new-run hadfs-exe -- ./mnt dc0.domain.alt # or just `dc0` without domain
```

or you can use `stack`

```sh
stack build
stack run ./mnt dc0
```

# Debugging
In case of wierd behavior you can build all with `profiling` option and then use RTS debug options
```sh
cabal new-configure --enable-profiling
cabal new-build
dist-newstyle/build/x86_64-linux/ghc-8.6.3/hadfs-0.1.0.0/x/hadfs-exe/build/hadfs-exe/hadfs-exe ./mnt dc0 +RTS -xc
```

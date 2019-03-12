# hadfs

# prepare, build and run
```sh
git clone https://github.com/omgbebebe/hadfs.git
cd hadfs
git submodule update --init --recursive
cabal new-build
mkdir ./mnt
cabal new-run hadfs-exe -- ./mnt dc0.domain.alt
```

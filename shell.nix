{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:
with nixpkgs;
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
          cabal-install stack 
        ]);
  libs = [openldap fuse cyrus_sasl ];
in
pkgs.stdenv.mkDerivation {
  name = "hadfs";
  buildInputs = [ ghc ] ++ libs;
  shellHook = ''
    eval $(egrep ^export ${ghc}/bin/ghc)
    LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${stdenv.lib.makeLibraryPath libs}"
    echo "HADFS nix dev environment"
  '';
}

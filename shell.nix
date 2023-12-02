{ pkgs ? import <nixpkgs> {} }:
let
  src = pkgs.nix-gitignore.gitignoreSource [] ./.;
  packageSet = pkgs.haskell.packages.ghc92;
  myPkg = packageSet.callCabal2nix "aoc23" src {};
in
pkgs.stdenv.mkDerivation {
  name = "aoc-shell";

  buildInputs = [
    myPkg.env.nativeBuildInputs

    packageSet.haskell-language-server
    packageSet.cabal-install
    packageSet.hlint
  ];
}

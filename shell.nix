{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc7103"
}:

with nixpkgs;

let ghc = haskell.packages.${compiler}.ghcWithPackages (x: with x; [
  ghc-mod
  parsec
  hdevtools
  hlint
  hoogle
  file-embed
]);

in stdenv.mkDerivation rec {
  name = "haskellDev";
  buildInputs = [haskellPackages.ghc];
  shellHook = "eval $(grep ^export ${ghc}/bin/ghc)";
}

{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/24.05.tar.gz") {} }:
pkgs.mkShell {
  packages = [ pkgs.cabal-install pkgs.haskell-language-server (pkgs.haskellPackages.ghcWithPackages (p: [p.hspec])) ];
}

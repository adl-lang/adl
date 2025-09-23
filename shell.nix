let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-25.05";
  pkgs = import nixpkgs { config = {}; overlays = []; };
in

pkgs.mkShell {
  packages = with pkgs; [
    cabal-install
    cmake
    deno
    haskell.compiler.ghc984Binary
    gradle
    jdk11
    rustup
    unzip
    yarn
    zip
  ];
}

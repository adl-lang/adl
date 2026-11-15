{
  description = "ADL (Algebraic Data Language) compiler";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);

      # adlc and the haskell packages it is built from.
      mkAdlc = pkgs:
        let
          hlib = pkgs.haskell.lib;
          # compiler/lib is a tree of symlinks to other parts of the repo, so
          # the source must include those.
          root = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.unions [
              ./haskell ./adl/stdlib ./rust/runtime ./typescript/runtime ./java/runtime/src
            ];
          };
          src = dir: "${root}/haskell/${dir}";
          hpkgs = pkgs.haskell.packages.ghc984.override {
            overrides = final: prev: {
              adl-lib0 = hlib.dontCheck (final.callCabal2nix "adl-lib0" (src "lib0") { });
              adl-runtime = hlib.dontCheck (final.callCabal2nix "adl-runtime" (src "runtime") { });
              adl-compiler = hlib.dontCheck (hlib.overrideCabal
                (final.callCabal2nix "adl-compiler" (src "compiler") { })
                (_: { src = root; sourceRoot = "${root.name}/haskell/compiler"; }));
            };
          };
        in
        hpkgs.adl-compiler;
    in
    {
      packages = forAllSystems (system: rec {
        adlc = mkAdlc nixpkgs.legacyPackages.${system};
        default = adlc;
      });

      apps = forAllSystems (system: rec {
        adlc = { type = "app"; program = "${self.packages.${system}.adlc}/bin/adlc"; };
        default = adlc;
      });

      overlays.default = final: prev: { adlc = mkAdlc final; };

      checks = forAllSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system}; adlc = self.packages.${system}.adlc; in {
          adlc-smoke = pkgs.runCommand "adlc-smoke" { } ''
            ${adlc}/bin/adlc show --version
            stdlib=$(${adlc}/bin/adlc show --adlstdlib)
            test -d "$stdlib"
            ${adlc}/bin/adlc verify --searchdir "$stdlib" sys.types
            touch $out
          '';
        });

      devShells = forAllSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system}; in {
          default = pkgs.mkShell {
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
          };
        });
    };
}

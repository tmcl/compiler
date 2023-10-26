{
  description = "Horrendous Haskell Compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

  };

  outputs = { self, nixpkgs, flake-utils,  ... }@inputs:

    let

      makePackage = pkgs: pkgs.haskellPackages.callPackage self { };
      overlay = import ./overlay.nix {
        inherit inputs;
      };
      pkgs = (import nixpkgs {
        system = "x86_64-linux";
        overlays = [ overlay ];
      });
    in {
      inherit overlay;
    } // flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      rec {

        devShell = let
          haskellDeps = ps: [
            ps.base
            ps.megaparsec
            ps.text
            ps.haskeline
            ps.mtl
            ps.tasty
            ps.tasty-hspec
            ps.hspec-megaparsec
          ];

        in pkgs.mkShell {
          shellHook = "exec ${pkgs.zsh}/bin/zsh -l";

          nativeBuildInputs = [
            (pkgs.haskellPackages.ghcWithHoogle haskellDeps)
            pkgs.cabal-install
            pkgs.haskell-language-server
            pkgs.ormolu
            pkgs.clang_16
            pkgs.nixfmt
            pkgs.ghcid
            pkgs.vscodium
          ];
        };
      });
}

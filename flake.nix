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
            ps.array
            ps.base
            ps.bytestring
            ps.containers
            ps.criterion
            ps.directory
            ps.filepath
            ps.llvm-hs
            ps.llvm-hs-pretty 
            ps.llvm-hs-pure 
            ps.megaparsec
            ps.mtl
            ps.optparse-applicative
            ps.parser-combinators
            ps.prettyprinter
            ps.pretty-simple
            ps.process
            ps.string-conversions
            ps.tasty
            ps.tasty-golden
            ps.tasty-hunit
            ps.text
            ps.unix

          ];

        in pkgs.mkShell {
          shellHook = "exec ${pkgs.zsh}/bin/zsh -l";

          nativeBuildInputs = [
            (pkgs.haskellPackages.ghcWithHoogle haskellDeps)
            pkgs.cabal-install
            pkgs.haskell-language-server
            pkgs.ormolu
            pkgs.clang_9
            pkgs.nixfmt
            pkgs.ghcid
            pkgs.vscodium
            pkgs.haskellPackages.happy
            pkgs.haskellPackages.alex
          ];
        };
      });
}

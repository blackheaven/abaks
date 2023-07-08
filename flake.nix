{
  description = "abaks";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    polysemy.url = github:polysemy-research/polysemy/16999eece5b10bc7c9cb8610be785f946e0140be;
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    # flake-utils.lib.eachDefaultSystem
    flake-utils.lib.eachSystem [ flake-utils.lib.system.x86_64-linux ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          jailbreakUnbreak = pkg:
            pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.unmarkBroken pkg));

          haskellPackages = pkgs.haskell.packages.ghc925.override {
            overrides = hself: hsuper: {
              type-errors = jailbreakUnbreak hsuper.type-errors;
              ListLike = jailbreakUnbreak hsuper.ListLike;
              polysemy = hsuper.callCabal2nix "polysemy" inputs.polysemy { };
              polysemy-plugin = hsuper.callCabal2nix "polysemy-plugin" "${inputs.polysemy}/polysemy-plugin" { };
            };
          };
          assets = builtins.filterSource (path: type: pkgs.lib.strings.hasInfix "assets" path) ./.;
        in
        rec
        {
          packages.abaks =
            haskellPackages.callCabal2nix "abaks" ./. rec {
              # Dependency overrides go here
            };

          defaultPackage = packages.abaks;

          devShell =
            let
              scripts = pkgs.symlinkJoin {
                name = "scripts";
                paths = pkgs.lib.mapAttrsToList pkgs.writeShellScriptBin {
                  ormolu-ide = ''
                    ${pkgs.ormolu}/bin/ormolu -o -XNoImportQualifiedPost -o -XOverloadedRecordDot $@
                  '';
                };
              };
            in
            pkgs.mkShell {
              buildInputs = with haskellPackages; [
                haskell-language-server
                ghcid
                cabal-install
                scripts
                ormolu
              ];
              inputsFrom = [
                self.defaultPackage.${system}.env
              ];
            };
        });
}

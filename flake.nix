{
  description = "abaks";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ flake-utils.lib.system.x86_64-linux ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          haskellPackages = pkgs.haskellPackages;
        in
        rec {
          packages.default =
            haskellPackages.callCabal2nix "abaks" ./. { };

          devShells.default =
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
                self.packages.${system}.default.env
              ];
            };
        });
}

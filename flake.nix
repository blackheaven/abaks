# https://levelup.gitconnected.com/using-nix-flakes-with-bazel-8189719ea799
{
  description =
    "Abaks is an Haskell/PureScript project for personal bank reconciliation statements";

  # Inputs
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.servant = {
    url = "github:haskell-servant/servant";
    flake = false;
  };
  inputs.servant-openapi3 = {
    url = "github:biocad/servant-openapi3";
    flake = false;
  };
  inputs.universum = {
    url = "github:aveltras/universum";
    flake = false;
  };
  inputs.easy-hls-src.url =
    "github:blackheaven/easy-hls-nix/c0fa0e71b6f2d9923d4b965ee29a48d80b859309";

  # outputs = all@{ self, c-hello, rust-web-server, nixpkgs, nix-bundle, ... }: {
  outputs = all@{ self, nixpkgs, flake-utils, easy-hls-src, servant
    , servant-openapi3, universum, ... }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;

      easy-hls = pkgs.callPackage easy-hls-src { ghcVersions = [ "9.2.1" ]; };

      haskellTool = tool:
        pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.${tool};
    in flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = {
          haskell = import ./api/deps.nix {
            servantSrcs = servant;
            servantOpenapi3Srcs = servant-openapi3;
            universumSrcs = universum;
          };
        };

        pkgs = builtins.foldl' (acc: overlay: acc.extend overlay)
          nixpkgs.legacyPackages.${system} (builtins.attrValues overlays);

      in {
        inherit overlays;

        # Utilized by `nix flake check`
        # checks.x86_64-linux.test = c-hello.checks.x86_64-linux.test;

        # Utilized by `nix build .`
        # defaultPackage.x86_64-linux = c-hello.defaultPackage.x86_64-linux;
        # packages.x86_64-linux.hello = c-hello.packages.x86_64-linux.hello;

        # Utilized by `nix run .#<name>`
        # apps.x86_64-linux.hello = {
        #   type = "app";
        #   program = c-hello.packages.x86_64-linux.hello;
        # };

        # Utilized by `nix bundle -- .#<name>` (should be a .drv input, not program path?)
        # bundlers.example = nix-bundle.defaultBundler;
        # defaultBundler = self.bundlers.example;

        # Utilized by `nix run . -- <args?>`
        # defaultApp.x86_64-linux = self.apps.x86_64-linux.hello;

        # Default overlay, for use in dependent flakes
        overlay = final: prev: { };

        # # Same idea as overlay but a list or attrset of them.

        # Utilized by `nix develop`
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.bazel_4
            pkgs.openjdk11
            easy-hls
            (haskellTool "ghcid")
            (haskellTool "hlint")
          ];
        };

        # devShells.x86_64-linux.example = self.devShell.x86_64-linux;
      });
}

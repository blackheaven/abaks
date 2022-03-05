{ ... }:
let
  flake = builtins.getFlake (toString ./.);
  baseNixpkgs = flake.inputs.nixpkgs.legacyPackages.${builtins.currentSystem};
  overlays = flake.overlays.${builtins.currentSystem};
  nixpkgs = builtins.foldl' (acc: overlay: acc.extend overlay) baseNixpkgs
    (builtins.attrValues overlays);
in nixpkgs

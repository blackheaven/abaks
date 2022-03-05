{ polysemyPkgs, servantSrcs, servantOpenapi3Srcs, universumSrcs }:
nixpkgsSelf: nixpkgsSuper:
(nixpkgsSuper.lib.composeManyExtensions [
  (self: super: {
    haskellPackages = super.haskell.packages.ghc921.override {
      overrides = hself: hsuper:
        let
          jailbreak = super.haskell.lib.doJailbreak;
          nocheck = self.haskell.lib.dontCheck;
          servantPkg = name:
            jailbreak (hsuper.callCabal2nix name "${servantSrcs}/${name}" { });
        in {
          polysemy = polysemyPkgs.polysemy-921;
          polysemy-plugin = polysemyPkgs.polysemy-plugin-921;
          servant = servantPkg "servant";
          servant-server = servantPkg "servant-server";
          # Conflicts
          servant-openapi3 = jailbreak
            (hsuper.callCabal2nix "servant-openapi3" servantOpenapi3Srcs { });
          universum =
            nocheck (hsuper.callCabal2nix "universum" universumSrcs { });
          hspec-wai = hsuper.hspec-wai_0_11_1;
          optics = hsuper.optics_0_4;
          optics-th = hsuper.optics-th_0_4;
          optics-extra = hsuper.optics-extra_0_4;
          optics-core = hsuper.optics-core_0_4;
          openapi3 = nocheck (jailbreak hsuper.openapi3);
        };
    };
  })
  (self: super: {
    ghc = super.haskellPackages.ghcWithPackages (hsPkgs:
      with hsPkgs; [
        aeson
        bytestring
        openapi3
        polysemy
        polysemy-plugin
        http-types
        network-uri
        warp
        wai-extra
        wai-cors
        servant
        servant-openapi3
        servant-server
        universum
      ]);
  })
]) nixpkgsSelf nixpkgsSuper

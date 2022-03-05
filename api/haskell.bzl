load(
    "@rules_haskell//haskell:defs.bzl",
    "ghc_plugin",
    "haskell_toolchain_library",
)

def toolchain(deps):
    for dep in deps:
        if dep != "base":
            haskell_toolchain_library(name=dep)
            if dep == "polysemy-plugin":
                ghc_plugin(
                    name = "polysemy_plugin",
                    module = "Polysemy.Plugin",
                    deps = ["polysemy", "polysemy-plugin"],
                )

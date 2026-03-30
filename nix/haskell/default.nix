{ inputs, ... }:

{
  imports = [
    inputs.haskell-flake.flakeModule
  ];

  perSystem =
    { config, ... }:
    {
      haskellProjects.default =
        {
          options,
          config,
          lib,
          pkgs,
          ...
        }:
        {
          projectRoot = lib.fileset.toSource {
            root = ../..;
            fileset = lib.fileset.unions [
              ../../app
              ../../src
              ../../test
              ../../nix2deb.cabal
              ../../cabal.project

              ../../CHANGELOG.md
              ../../LICENSE
              ../../README.md

              ../../bindings-nix
            ];
          };
          settings = {
            nix2deb = {
              separateBinOutput = true;
              generateOptparseApplicativeCompletions = config.packages.nix2deb.cabal.executables;
              custom =
                pkg:
                pkgs.haskell.lib.compose.overrideCabal (old: {
                  executableToolDepends = old.executableToolDepends or [ ] ++ [ pkgs.makeWrapper ];
                  postInstall =
                    old.postInstall or ""
                    + "\n"
                    + ''
                      wrapProgram "''${!outputInfo}/bin/nix2deb" --suffix PATH : "${
                        lib.makeBinPath [
                          pkgs.binutils
                          pkgs.dpkg
                          pkgs.glibc
                          pkgs.patchelf
                        ]
                      }"
                    '';
                }) pkg;
            };
            bindings-nix = {
              # TODO enable it by change haskell-flake and/or hs-bindgen
              buildFromSdist = false; # otherwise, generateBindings from hs-bindgen fails
              custom =
                pkg:
                pkg.override {
                  nix = pkgs.nix; # we do not want haskellPackages.nix
                }
                # TODO upstream generateBindings to haskell-flake so that
                # we can move generateBindings into ./hs-bindgen.nix from this `custom`
                |> pkgs.haskell.lib.compose.generateBindings ../../bindings-nix/hs-bindgen/generate-bindings;
            };
          };
          devShell.tools = hpkgs: {
            inherit (hpkgs) cabal-gild; # needed by HLS
          };
          autoWire = builtins.filter (attr: attr != "devShells") options.autoWire.default;
          # ghc 9.10 has bad HLS support: for example, hlint is broken
          basePackages =
            assert lib.assertMsg (lib.versionOlder pkgs.haskellPackages.ghc.version "9.12")
              "remove basePackages config, use the default pkgs.haskellPackages";
            pkgs.haskell.packages.ghc912;
        };

      packages.default = config.packages.nix2deb;
      apps.default = config.apps.nix2deb;
    };
}

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
            root = ../.;
            fileset = lib.fileset.unions [
              ../app
              ../src
              ../test
              ../nix2deb.cabal
              ../cabal.project

              ../CHANGELOG.md
              ../LICENSE
              ../README.md
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
                          pkgs.patchelf
                        ]
                      }"
                    '';
                }) pkg;
            };
          };
          devShell.tools = hpkgs: {
            inherit (hpkgs) cabal-gild; # needed by HLS
          };
          autoWire = builtins.filter (attr: attr != "devShells") options.autoWire.default;
        };

      packages.default = config.packages.nix2deb;
      apps.default = config.apps.nix2deb;
    };
}

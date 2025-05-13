{
  outputs =
    { self, nixpkgs }:
    let
      projectName = "nix2deb";

      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = { };
        overlays = [ self.overlays.default ];
      };
      inherit (pkgs) haskellPackages;
    in
    {
      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override (args: {
          overrides = final.lib.composeExtensions args.overrides (
            let
              hlib = final.haskell.lib.compose;
            in
            hfinal: hprev: {
              ${projectName} =
                let
                  # filter out haskell-unrelated files to avoid unnecessary rebuilds
                  src = builtins.path {
                    path = ./.;
                    # NOTE setting name is important because the default one contains
                    # the store path of this flake, which defeats the motivation
                    name = "source";
                    filter =
                      path: type:
                      !builtins.elem (builtins.baseNameOf path) [
                        ".dir-locales.el"
                        ".gitignore"
                        ".hlint.relude.yaml"
                        ".hlint.yaml"
                        "flake.lock"
                        "flake.nix"
                        "hie.yaml"
                      ];
                  };
                in
                hfinal.callCabal2nix projectName src { };
              cabal-gild-multi-file-wrapper = final.writeShellApplication {
                name = "cabal-gild-multi-file-wrapper";
                runtimeInputs = [ hfinal.cabal-gild ];
                text = ''for file in "''${@}"; do cabal-gild --io "$file"; done'';
              };
            }
          );
        });
      };
      packages.${system} = {
        default = self.packages.${system}.wrappedBin;
        bin = haskellPackages.generateOptparseApplicativeCompletions [
          projectName
        ] (pkgs.haskell.lib.compose.justStaticExecutables haskellPackages.${projectName});
        wrappedBin = pkgs.haskell.lib.compose.overrideCabal (old: {
          # nativeBuildInputs = old.nativeBuildInputs or [ ] ++ [ pkgs.makeWrapper ];
          executableToolDepends = old.executableToolDepends or [ ] ++ [ pkgs.makeWrapper ];
          postInstall =
            old.postInstall or ""
            + "\n"
            + ''
              wrapProgram "$out/bin/${projectName}" --suffix PATH : "${
                pkgs.lib.makeBinPath [
                  pkgs.binutils
                  pkgs.dpkg
                  pkgs.patchelf
                ]
              }"
            '';
        }) self.packages.${system}.bin;
        testExe = pkgs.stdenv.mkDerivation {
          pname = "test-exe";
          version = "0.1.0";

          src = pkgs.emptyDirectory;

          buildInputs = [ pkgs.libev ];

          postBuild = ''
            cat > test-exe.c <<'EOF'
            #include <stdio.h>

            #include <ev.h>

            int
            main (void)
            {
              printf ("libev version: %d.%d\n", ev_version_major (), ev_version_minor ());
              printf ("fn addr: %p\n", (void *) ev_invoke);
              return 0;
            }
            EOF
            $CC -o test-exe -lev test-exe.c
          '';

          postInstall = ''
            install -D -t $out/bin test-exe
          '';

          meta = {
            description = "Test executable for nix2deb";
            maintainers = [ pkgs.lib.maintainers.linj ];
          };
        };
      };
      devShells.${system}.default = haskellPackages.shellFor {
        packages = hpkgs: [ hpkgs.${projectName} ];
        nativeBuildInputs = with haskellPackages; [
          cabal-gild # needed by HLS
          cabal-install
          ghcid
          haskell-language-server
          (pkgs.nixfmt-tree.override {
            runtimeInputs = [
              cabal-gild-multi-file-wrapper
              ormolu
            ];
            settings = {
              formatter = {
                cabal-gild = {
                  command = "cabal-gild-multi-file-wrapper";
                  includes = [
                    "*.cabal"
                    "cabal.project*"
                  ];
                };
                ormolu = {
                  command = "ormolu";
                  options = [
                    "--mode"
                    "inplace"
                    "--check-idempotence"
                  ];
                  includes = [ "*.hs" ];
                };
              };
            };
          })
        ];
        withHoogle = true;
        doBenchmark = true;
      };
    };
}

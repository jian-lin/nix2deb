{
  # use haskell-updates branch for HLS-2.10.0.0
  inputs.nixpkgs.url = "github:nixos/nixpkgs/haskell-updates";

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
              haskell-language-server = hlib.appendPatch (final.fetchpatch2 {
                name = "provide-code-action-in-eval-plugin.patch";
                url = "https://github.com/haskell/haskell-language-server/commit/caa1c32f4b45c3717dadc00fea2b489f3af2e616.patch";
                excludes = [
                  ".pre-commit-config.yaml"
                  "docs/features.md"
                  "plugins/hls-eval-plugin/README.md"
                ];
                hash = "sha256-PwuMYUWI9bK9hErz0nS8ETCE5xxRn5E/XdXKxZphLxM=";
              }) hprev.haskell-language-server;
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
            }
          );
        });
      };
      packages.${system} = {
        default = pkgs.lib.getBin self.packages.${system}.all;
        all = haskellPackages.generateOptparseApplicativeCompletions [
          projectName
        ] (pkgs.haskell.lib.compose.enableSeparateBinOutput haskellPackages.${projectName});
      };
      devShells.${system}.default = haskellPackages.shellFor {
        packages = hpkgs: [ hpkgs.${projectName} ];
        nativeBuildInputs = with haskellPackages; [
          cabal-gild
          cabal-install
          ghcid
          haskell-language-server
          (pkgs.nixfmt-tree.override {
            settings = {
              formatter = {
                cabal-gild = {
                  command = "cabal-gild";
                  options = [ "--io" ];
                  includes = [
                    "*.cabal"
                    "cabal.project"
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

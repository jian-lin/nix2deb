{ inputs, ... }:

{
  imports = [
    inputs.treefmt-nix.flakeModule
  ];

  perSystem =
    { config, ... }:
    {
      treefmt = {
        # nix
        programs = {
          deadnix.enable = true;
          nixf-diagnose.enable = true;
          nixfmt.enable = true;
          statix.enable = true;
        };

        # haskell
        programs = {
          ormolu = {
            enable = true;
            package = config.haskellProjects.default.outputs.finalPackages.ormolu;
          };
          yamlfmt = {
            enable = true;
            excludes = [
              ".hlint.relude.yaml" # directly copied from relude repo
            ];
          };
          hlint = {
            enable = true;
            package = config.haskellProjects.default.outputs.finalPackages.hlint;
          };
        };
        settings.formatter.hlint.options = [
          "-XGHC2021"
          "-XBlockArguments"
          "--hint=.hlint.relude.yaml"
          "--hint=.hlint.yaml"
          "--color=always"
        ];

        # c
        programs.clang-format.enable = true;
        settings.formatter.clang-format.options = [ "--style=GNU" ];

        programs.keep-sorted.enable = true;
      };
    };
}

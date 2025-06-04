{
  perSystem =
    { pkgs, config, ... }:
    {
      # TODO delete this file and config it with other treefmt formatters when it is upstreamed
      # https://github.com/numtide/treefmt-nix/pull/361
      treefmt.settings.formatter.cabal-gild = {
        command = pkgs.writeShellApplication {
          name = "cabal-gild-wrapper";
          runtimeInputs = [ config.haskellProjects.default.outputs.finalPackages.cabal-gild ];
          text = ''
            for file in "$@"; do
              cabal-gild --io="$file"
            done
          '';
        };
        includes = [
          "*.cabal"
          "cabal.project"
          "cabal.project.local"
        ];
      };
    };
}

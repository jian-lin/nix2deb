{
  perSystem =
    {
      config,
      pkgs,
      ...
    }:
    {
      devShells.default = pkgs.mkShellNoCC {
        inputsFrom = [
          # keep-sorted start block=yes
          config.haskellProjects.default.outputs.devShell
          config.pre-commit.devShell
          config.treefmt.build.devShell
          # keep-sorted end
        ];
        packages = [
          # keep-sorted start block=yes
          pkgs.binutils
          pkgs.dpkg
          pkgs.patchelf
          # keep-sorted end
        ];
      };
    };
}

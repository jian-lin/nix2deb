{ inputs, ... }:

{
  imports = [
    inputs.git-hooks-nix.flakeModule
  ];

  perSystem = {
    pre-commit = {
      settings.hooks = {
        # keep-sorted start block=yes
        markdownlint.enable = true;
        treefmt.enable = true;
        # keep-sorted end
      };
    };
  };
}

{ inputs, ... }:

{
  imports = [
    inputs.git-hooks-nix.flakeModule
  ];

  perSystem = {
    pre-commit = {
      settings.hooks = {
        # keep-sorted start block=yes
        markdownlint = {
          enable = true;
          settings.configuration = {
            MD013.code_block_line_length = 100;
          };
        };
        treefmt.enable = true;
        # keep-sorted end
      };
    };
  };
}

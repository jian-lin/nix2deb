{
  description = "A flake to develop and run nix2deb";

  inputs = {
    # keep-sorted start block=yes
    flake-parts.url = "github:hercules-ci/flake-parts";
    git-hooks-nix = {
      url = "github:cachix/git-hooks.nix";
      inputs = {
        flake-compat.follows = "";
        gitignore.follows = "";
        nixpkgs.follows = "nixpkgs";
      };
    };
    haskell-flake.url = "github:srid/haskell-flake";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # keep-sorted end
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } (
      { lib, ... }:
      {
        imports = lib.fileset.toList (lib.fileset.fileFilter (file: file.hasExt "nix") ./nix);
        systems = lib.systems.flakeExposed;
        # my.debug = {
        #   enable = true;
        #   currentSystem = "x86_64-linux";
        # };
      }
    );
}

{ lib, config, ... }:

{
  options.my.debug = {
    enable = lib.mkEnableOption "flake debug with workarounds for IFD failures in `nix flake show`";
    currentSystem = lib.mkOption {
      type = lib.types.enum lib.systems.flakeExposed;
    };
  };

  config = lib.mkIf config.my.debug.enable {
    debug = true;
    systems = lib.mkForce (lib.singleton config.my.debug.currentSystem);
  };
}

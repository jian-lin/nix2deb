{ inputs, ... }:

{
  perSystem =
    { system, ... }:
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        config = { };
        overlays = [ inputs.hs-bindgen.overlays.default ];
      };
    };
}

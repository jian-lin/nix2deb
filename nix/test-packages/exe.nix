{
  perSystem =
    { pkgs, ... }:
    {
      packages.testExe = pkgs.callPackage (
        {
          stdenv,
          libev,
        }:

        stdenv.mkDerivation {
          pname = "test-exe";
          version = "0.1.0";

          src = ./exe.c;

          dontUnpack = true;

          buildInputs = [ libev ];

          postBuild = ''
            $CC -o test-exe -lev $src
          '';

          postInstall = ''
            install -D -t $out/bin test-exe
          '';

          meta = {
            description = "Test executable for nix2deb";
          };
        }
      ) { };
    };
}

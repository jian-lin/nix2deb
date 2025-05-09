# Nix2deb

Convert a Nix package to a deb package using other deb packages as dependencies.

## Example usage

Run this command to generate a test deb package.

```bash
nix run github:jian-lin/nix2deb -- \
  --nix-eval-command 'nix eval github:jian-lin/nix2deb#testExe' \
  --suite plucky \
  $(nix build github:jian-lin/nix2deb#testExe --print-out-paths --no-link)
```

You can inspect the generated deb package using `dpkg-deb` command.

```bash
dpkg-deb --info test-exe.deb
dpkg-deb --contents test-exe.deb
```

To test the generated package, copy it to a Ubuntu system and run the following commands:

```bash
# install
sudo apt install ./test-exe.deb
# verify the installed binary works
test-exe
# optional: uninstall
sudo apt remove test-exe && sudo apt autoremove
```

## Manual

```console
Usage: nix2deb DIRECTORY (-e|--nix-eval-command COMMAND) (-s|--suite CODENAME) 
               [-a|--arch ARCH] [-n|--maintainer-name|--name NAME] 
               [-m|--email|--maintainer-email EMAIL] [-l|--log-level LOG-LEVEL]

  Convert a Nix package to a deb package using other deb packages as
  dependencies

Available options:
  DIRECTORY                Directory of your built nix package such as ./result
  -e,--nix-eval-command COMMAND
                           Command to eval your nix package such as 'nix eval
                           .#myPkg'
  -s,--suite CODENAME      Version of the Ubuntu system such as plucky
  -a,--arch ARCH           [Amd64,Arm64] (default: Amd64)
  -n,--maintainer-name,--name NAME
                           Maintainer name of this package
                           (default: nix2deb user)
  -m,--email,--maintainer-email EMAIL
                           Maintainer email of this package
                           (default: someone@example.com)
  -l,--log-level LOG-LEVEL [Debug,Info,Warning,Error] (default: Info)
  -h,--help                Show this help text
```

## License

AGPL-3.0-or-later

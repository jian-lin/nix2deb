# Nix2deb

Convert a Nix package to a deb package using other deb packages as dependencies.

## Example usage

Run this command to generate a test deb package.

```bash
nix run github:jian-lin/nix2deb -- \
  --nix-installable github:jian-lin/nix2deb#testExe \
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
Usage: nix2deb DIRECTORY (-i|--nix-installable INSTALLABLE)
               (-s|--suite CODENAME) [-a|--arch ARCH]
               [-c|--choose-strategy|--multiple-deb-dependency-package-choose-strategy STRATEGY]
               [-n|--maintainer-name|--name NAME]
               [-e|--email|--maintainer-email EMAIL] [-t|--scrape-threads N]
               [-l|--log-level LOG-LEVEL]

  Convert a Nix package to a deb package using other deb packages as
  dependencies

Available options:
  DIRECTORY                Directory of your built nix package such as ./result
  -i,--nix-installable INSTALLABLE
                           Nix installable representing your nix package such as
                           .#myPkg. See
                           https://nix.dev/manual/nix/latest/command-ref/new-cli/nix.html#installables.
  -s,--suite CODENAME      Version of the Ubuntu system such as plucky
  -a,--arch ARCH           [Amd64,Arm64] (default: Amd64)
  -c,--choose-strategy,--multiple-deb-dependency-package-choose-strategy STRATEGY
                           [Heuristic,ErrorOut] (default: Heuristic)
  -n,--maintainer-name,--name NAME
                           Maintainer name of this package
                           (default: nix2deb user)
  -e,--email,--maintainer-email EMAIL
                           Maintainer email of this package
                           (default: someone@example.com)
  -t,--scrape-threads N    Number of threads to scrape concurrently (default: 3)
  -l,--log-level LOG-LEVEL [Debug,Info,Warning,Error] (default: Info)
  -h,--help                Show this help text
```

## License

MIT

# nix2deb

Convert a Nix package to a deb package using other deb packages as dependencies.

In other words, like [nix-bundle][], but with its [#93][nix-bundle-93] fixed.

[nix-bundle]: https://github.com/nix-community/nix-bundle
[nix-bundle-93]: https://github.com/nix-community/nix-bundle/issues/93

## Usage

Generate a test deb package:

```bash
nix run github:jian-lin/nix2deb -- \
  --nix-installable github:jian-lin/nix2deb#testExe \
  --suite plucky \
  $(nix build github:jian-lin/nix2deb#testExe --print-out-paths --no-link)
```

Optionally, inspect the generated deb package using `dpkg-deb` command:

```bash
dpkg-deb --info test-exe.deb
dpkg-deb --contents test-exe.deb
```

Test the generated package by copying it to Ubuntu and running these commands:

```bash
# install
sudo apt install ./test-exe.deb
# verify the installed binary works
test-exe
# optional: uninstall
sudo apt remove test-exe && sudo apt autoremove
```

## Manual

Run `nix2deb` with `--help` to see the manual:

```bash
nix run github:jian-lin/nix2deb -- --help
```

## Development

Enter the development environment powered by [Nix][nix]:

```bash
nix develop
```

[nix]: https://nixos.org/download/#download-nix

## License

MIT

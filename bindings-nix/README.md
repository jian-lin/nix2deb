# bindings-nix

Bindings to [Nix C API][nix-c-api] using [hs-bindgen][]

[nix-c-api]: https://nix.dev/manual/nix/latest/c-api.html
[hs-bindgen]: https://github.com/well-typed/hs-bindgen

## Development

1. Enter the development environment: `nix develop`
1. Generate bindings: `./hs-bindgen/generate-bindings`
1. Do the usual Haskell development

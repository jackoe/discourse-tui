This is a terminal viewer for [discourse](https://www.discourse.org/) inspired by a [similar tool for reddit](https://github.com/michael-lazar/rtv).

![demo](demo.svg)

Usage: discourse-tui baseUrl
Example: discourse-tui discourse.haskell.org

to compile:
(make sure you have nix installed)[https://nixos.org/nix/download.html]
```
nix-build release.nix
```

# Top-level Nix expression returning a complete nixpkgs that includes Studio.
let overlays = import ./overlays.nix;
    # base on nixpkgs release-17.03 from 15-07-2016.
    nixpkgs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/18.03.tar.gz);
in
nixpkgs { overlays = overlays; }


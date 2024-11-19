#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#nvd --command bash

if [ -e result ]; then
    mv result result-prev
fi

nix flake update

nix build

if [ -e result-prev ]; then
    nvd diff result-prev result
fi

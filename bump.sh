#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#nix-output-monitor nixpkgs#nvd --command bash
set -ex

if [ -e result ]; then
    rm -f result-prev
    mv result result-prev
fi

nix flake update

nom build --keep-going

if [ -e result-prev ]; then
    nvd diff result-prev result
fi

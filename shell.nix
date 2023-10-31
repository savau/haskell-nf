# SPDX-FileCopyrightText: 2023 Sarah Vaupel <sarah.vaupel@uniworx.de>
#
# SPDX-License-Identifier: AGPL-3.0-or-later

{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs.lib) optionalString;
  haskellPackages = pkgs.haskellPackages;
in pkgs.mkShell {
  name = "haskell-nf";
  nativeBuildInputs = (with pkgs;
    [
      stack
      ghc
      reuse
      pre-commit
    ]
  ) ++ (with pkgs.haskellPackages;
    [
      hlint
    ]
  );
}

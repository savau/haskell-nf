# SPDX-FileCopyrightText: 2023 Sarah Vaupel <sarah.vaupel@uniworx.de>
#
# SPDX-License-Identifier: AGPL-3.0-or-later

{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "23.05";
    };

    flake-utils = {
      type = "github";
      owner = "numtide";
      repo = "flake-utils";
      ref = "main";
    };

    haskell-nix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, haskell-nix, ... }: flake-utils.lib.eachSystem ["x86_64-linux"]
    (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
        };

        overlays = [
          # (final: prev:
          #    let
          #      pkgs = import nixpkgs { inherit system; };
          #      # ghc = nur-no-pkgs.repos.mpickering.ghc.ghc8101;
          #    in {
          #      stack = pkgs.symlinkJoin {
          #        # inherit ghc;
          #        inherit (pkgs.stack) name;
          #        paths = [pkgs.stack pkgs.ghc];
          #        nativeBuildInputs = [pkgs.makeWrapper];

          #        postBuild = ''
          #          wrapProgram $out/bin/stack \
          #            --prefix PATH : "${prev.lib.makeBinPath [pkgs.nix]}" \
          #            --add-flags "\
          #              --nix \
          #              --nix-shell-file=${./stack.nix} \
          #              --nix-path=nixpkgs=${nixpkgs} \
          #            "
          #        '';
          #      };
          #    }
          # )
          haskell-nix.overlay
        ];

        inherit (pkgs.lib) recursiveUpdate;
      in {
        devShell = import ./shell.nix { pkgs = self.legacyPackages.${system}; };

        legacyPackages = pkgs.lib.foldr (overlay: acc: acc // recursiveUpdate (overlay self.legacyPackages.${system} pkgs) pkgs) {} overlays;
      }
    );
}

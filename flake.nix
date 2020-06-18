{
  description = "jekor.com";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-20.03;
  inputs.utils.url = github:numtide/flake-utils;
  inputs.gressgraph.url = github:jekor/gressgraph;
  inputs.jcoreutils.url = github:jekor/jcoreutils;
  inputs.jigplate.url = github:jekor/jigplate;
  inputs.jsonwrench.url = github:jekor/jsonwrench;

  outputs = {self, nixpkgs, utils, gressgraph, jcoreutils, jigplate, jsonwrench}:
    utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in rec {
        defaultPackage = pkgs.callPackage ./default.nix {
          gressgraph = gressgraph.defaultPackage.${system};
          jcoreutils = jcoreutils.defaultPackage.${system};
          jigplate = jigplate.defaultPackage.${system};
          jsonwrench = jsonwrench.defaultPackage.${system};
        };
      }
    );
}

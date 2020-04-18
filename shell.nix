{ ghc }:
let
  rev = "cd33f3aaeb1b859acd7b5b931d89dc3c141e0a7d";
  sha256 = "053vqbv5khsh1as848315f8ldk7dvv4pwfxhg65qp500jn3a7xgs";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = sha256;
  };
  config = {
    packageOverrides = pkgs: rec {
      mecab = pkgs.callPackage ./nix/mecab.nix { inherit mecab-ipadic; };
      mecab-ipadic = pkgs.callPackage ./nix/mecab-ipadic.nix { inherit mecab-nodic; };
      mecab-nodic = pkgs.callPackage ./nix/mecab-nodic.nix {};
    };
  };
  pkgs = import nixpkgs { inherit config; };
in
  with pkgs;
  haskell.lib.buildStackProject {
    inherit ghc;
    buildInputs = [ gmp6 libffi libgcc mecab zlib ];
    name = "markov-bot";
  }

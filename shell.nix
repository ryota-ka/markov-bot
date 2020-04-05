{ghc}:
let
  rev = "cd33f3aaeb1b859acd7b5b931d89dc3c141e0a7d";
  sha256 = "053vqbv5khsh1as848315f8ldk7dvv4pwfxhg65qp500jn3a7xgs";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = sha256;
  };
  pkgs = import nixpkgs {};
in
  with pkgs;
  haskell.lib.buildStackProject {
    inherit ghc;
    buildInputs = [ gmp6 libffi libgcc mecab zlib ];
    name = "markov-bot";
  }

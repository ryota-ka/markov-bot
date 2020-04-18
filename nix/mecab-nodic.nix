{ stdenv, fetchurl }:
let
  mecab-base = import ./mecab-base.nix { inherit fetchurl; };
in
stdenv.mkDerivation (
  mecab-base // {
    name = "mecab-nodic-${mecab-base.version}";
  }
)

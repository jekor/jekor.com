{ stdenv, bash, coreutils, findutils, gnugrep, graphviz, pandoc, makeWrapper
, jcoreutils, jigplate, jsonwrench, gressgraph
}:

stdenv.mkDerivation {
  name = "jekor.com";
  src = ./.;
  buildInputs = [ bash pandoc makeWrapper jcoreutils jigplate jsonwrench ];
  inherit bash coreutils findutils gnugrep graphviz jcoreutils jsonwrench gressgraph;
  builder = ./builder.sh;
}

{ stdenv, bash, coreutils, findutils, gnugrep, graphviz, pandoc, makeWrapper
, jcoreutils, jigplate, jsonwrench, gressgraph, toplevel
, src ? ./.
}:

stdenv.mkDerivation {
  inherit src;
  name = "jekor.com";
  buildInputs = [ bash pandoc makeWrapper jcoreutils jigplate jsonwrench ];
  inherit bash coreutils findutils gnugrep graphviz jcoreutils jsonwrench gressgraph toplevel;
  builder = ./builder.sh;
}

{ stdenv, fetchurl, bash, coreutils, findutils, gnugrep, graphviz, pandoc, makeWrapper, jcoreutils, jigplate, jsonwrench, gressgraph }:

let
  jquery = fetchurl {
    url = http://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js;
    sha256 = "1d000mm39r2jr1sa44jwkbig8hd2japjfkgangamm05nik78vdj7";
  };
in stdenv.mkDerivation {
  name = "jekor.com";
  src = ./.;
  buildInputs = [ bash pandoc makeWrapper jcoreutils jigplate jsonwrench ];
  inherit bash coreutils findutils gnugrep graphviz jquery jcoreutils jsonwrench gressgraph;
  builder = ./builder.sh;
}

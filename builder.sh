source $stdenv/setup

cp -r $src src
chmod -R u+w src
pushd src
make
popd
mv src $out
wrapProgram "$out/bin/post-comment" --prefix PATH : "$bash/bin:$findutils/bin:$gnugrep/bin:$coreutils/bin:$jcoreutils/bin:$jsonwrench/bin" --set toplevel $toplevel
wrapProgram "$out/www/gressgraph/graph/POST" --prefix PATH : "$bash/bin:$coreutils/bin:$graphviz/bin:$gressgraph/bin"

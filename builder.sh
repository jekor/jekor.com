source $stdenv/setup

cp -r $src src
chmod -R u+w src
pushd src
make
popd
mv src/www $out
wrapProgram "$out/post-comment" --prefix PATH : "$bash/bin:$findutils/bin:$gnugrep/bin:$coreutils/bin:$jcoreutils/bin:$jsonwrench/bin"
wrapProgram "$out/gressgraph/graph/POST" --prefix PATH : "$bash/bin:$coreutils/bin:$graphviz/bin:$gressgraph/bin"
mkdir -p $out/script/jquery
cp $jquery $out/script/jquery/application.javascript

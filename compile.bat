
del Release
copy *.ml? Release

ocamlyacc -bRelease/parser parser.mly
ocamllex lexer.mll -o Release/lexer.ml

cd Release
ocamlopt -I ../camlbz2 -a -o radis.cmxa utilio.ml equity.ml util.ml siarchive.ml fieldindex.ml args.ml eqloader.ml correct.ml passdata.ml op.ml parser.mli parser.ml lexer.ml compiler.ml stats.ml dbf.ml dbfreader.ml ../camlbz2/camlbz2.mli

for %%f in (bt btall pick pickall analyze analyze5 fields scprice dbdel dbinit dblist dbload) do ocamlopt -o ../%%f.exe str.cmxa unix.cmxa radis.cmxa %%f.ml ../camlbz2/libcamlbz2.lib -cclib -Llibs
cd ..

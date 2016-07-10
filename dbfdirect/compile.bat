
del Release
copy ..\*.ml? Release

del Release\siarchive.ml
copy siarchive.ml Release

ocamlyacc -bRelease/parser ../parser.mly
ocamllex ../lexer.mll -o Release/lexer.ml

cd Release
ocamlopt -a -o radis.cmxa utilio.ml equity.ml util.ml dbf.ml siarchive.ml fieldindex.ml args.ml eqloader.ml correct.ml passdata.ml op.ml parser.mli parser.ml lexer.ml compiler.mli compiler.ml stats.ml

for %%f in (bt btall pick pickall analyze analyze5 fields scprice) do ocamlopt -o ../%%f.exe str.cmxa unix.cmxa radis.cmxa %%f.ml
cd ..

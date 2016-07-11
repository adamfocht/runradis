#!/bin/bash

rm -Rf Release/*
cp *.ml* Release/

ocamlyacc -bRelease/parser parser.mly
ocamllex lexer.mll -o Release/lexer.ml

cd Release

echo "Compiling radis.cxma"
ocamlopt -I ../camlbz2 -a -o radis.cmxa ../camlbz2/camlbz2.mli utilio.ml equity.ml util.ml siarchive.ml fieldindex.ml args.ml eqloader.ml correct.ml passdata.ml op.ml compiler.mli parser.mli parser.ml lexer.ml compiler.ml stats.ml dbf.ml dbfreader.ml

for f in bt btall pick pickall analyze analyze5 fields scprice dbdel dbinit dblist dbload; do echo "Compiling "$f; ocamlopt -o $f unix.cmxa str.cmxa radis.cmxa $f.ml ../camlbz2/camlintf.o -cclib '-Llibs -lbz2'; done

cd ..

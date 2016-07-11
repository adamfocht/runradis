#!/bin/bash

ocamlopt -c camlbz2.mli
ocamlopt camlintf.c
#ocamlopt -I include -c camlintf.c blocksort.c bzlib.c compress.c crctable.c
#ocamlopt -I include -c decompress.c huffman.c randtable.c

#ocamlmklib -o camlbz2 camlintf.obj blocksort.obj bzlib.obj compress.obj crctable.obj decompress.obj huffman.obj randtable.obj

# OCAML

 .ml : source code
 .mli : interface file
 .cmi : compiled interface file
 .cmo : object bytecode
 .cmx : native compilation info
 .o : native code compiled

 .ml + .mli = a compilation unit
 .cma = lib
 .cmxa = compiled lib

build order :
 1 - source with ocamlc/ocamlopt -c test.ml
 2 - then we link with ocamlc/ocamlopt

RESULT = a.out
SOURCES = filter.ml main.ml
PACKS = extlib
INCDIRS = ~/src/ocaml-mylib
LIBS = mylib
ANNOTATE = yes

all: debug-code

export OCAMLMAKEFILE = ~/tmp/ocaml/OCamlMakefile
include $(OCAMLMAKEFILE)

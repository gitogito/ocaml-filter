RESULT = a.out
SOURCES = util.ml base.ml i0o1.ml i1o1.ml generator.ml btfilter.ml filter.ml main.ml
PACKS = extlib
INCDIRS = ~/src/ocaml-mylib
LIBS = mylib
ANNOTATE = yes
OCAMLFLAGS = -bin-annot -w A-44

all: debug-code

export OCAMLMAKEFILE = ~/src/ocamlmakefile/OCamlMakefile
include $(OCAMLMAKEFILE)

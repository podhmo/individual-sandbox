build:
	ocamlopt -annot f.ml
	ocamlopt -annot g.ml
	ocamlopt -annot h.ml
clean:
	ls * | grep -v "\.ml$$" | grep -v "\.annot$$" | grep -v Makefile | xargs rm

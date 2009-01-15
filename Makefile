arld: arld.cmx
	ocamlopt.opt -o $@ unix.cmxa $<
arld.cmx: arld.ml
	ocamlopt.opt -dtypes -o $@ -c $<

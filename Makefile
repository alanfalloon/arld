arln: arln.cmx
	ocamlopt.opt -o $@ unix.cmxa $<
arln.cmx: arln.ml
	ocamlopt.opt -dtypes -o $@ -c $<

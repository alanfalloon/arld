arln: arln.cmx
	ocamlopt.opt -o $@ $<
arln.cmx: arln.ml
	ocamlopt.opt -o $@ -c $<

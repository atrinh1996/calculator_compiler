# comment
toplevel.native: ast.ml parser.mly scanner.mll toplevel.ml
	ocamlbuild toplevel.native

clean:
	ocamlbuild -clean
	rm -rf parser.ml parser.mli scanner.ml toplevel.native
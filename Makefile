all:
	ocamlbuild expr.byte

clean:
	rm -rf _build/
	rm *.byte

all:
	corebuild -pkg ocamlgraph expr.byte

clean:
	rm -rf _build/
	rm *.byte

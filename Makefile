all:
	corebuild -pkg ocamlgraph main.byte

clean:
	rm -rf _build/
	rm *.byte

.PHONY: build install uninstall doc clean

build:
	dune build @install
	ln -sf _build/install/default/bin bin

install:
	dune install

uninstall:
	dune uninstall

doc:
	dune build @doc
	ln -sf _build/default/_doc doc

clean:
	dune clean
	rm -f bin doc

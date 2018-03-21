.PHONY: build install uninstall doc clean

build:
	jbuilder build @install
	ln -sf _build/install/default/bin bin

install:
	jbuilder install

uninstall:
	jbuilder uninstall

doc:
	jbuilder build @doc
	ln -sf _build/default/_doc doc

clean:
	jbuilder clean
	rm -f bin doc

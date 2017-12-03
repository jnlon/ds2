all:
	eval `opam config env`; \
	ocamlbuild -pkg unix ds2.native
clean:
	ocamlbuild -clean

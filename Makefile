.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

run:
	OCAMLRUNPARAM=b dune exec bin/run.exe

repl:
	OCAMLRUNPARAM=b dune exec bin/repl.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f rml.zip
	zip -r rml.zip . -x@exclude.lst

clean:
	dune clean
	rm -f rml.zip

doc:
	dune build @doc

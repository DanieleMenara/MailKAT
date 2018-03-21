OCB := ocamlbuild -use-menhir -tag thread -use-ocamlfind -pkg core -package str

EXAMPLEDIR:=./examples
SIEVE:=/src/compiler.native

all:
	$(OCB) src/compiler.native

tests: all sieve-tests

%.native:
	$(OCB) "$@"

clean:
	$(OCB) -clean

VPATH = src/

sieve-tests:
	-@for TST in $(EXAMPLEDIR)/*.txt ; do echo $$TST: ; _build/$(SIEVE) $$TST ; echo ; done

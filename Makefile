OCB := ocamlbuild -use-menhir -tag thread -use-ocamlfind -pkg core -package str

EXAMPLEDIR:=./examples
SIEVE:=/src/mailkat.native

all:
	$(OCB) src/mailkat.native

tests: all sieve-tests

%.native:
	$(OCB) "$@"

clean:
	$(OCB) -clean

clean-tests:
	rm -f *.sieve

sieve-tests:
	-@for TST in $(EXAMPLEDIR)/*.txt ; do echo $$TST: ; _build/$(SIEVE) -to-sieve $$TST ; echo ; done

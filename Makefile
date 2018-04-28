OCB := ocamlbuild -use-menhir -tag thread -use-ocamlfind -pkg core -package str

EXAMPLEDIR:=./examples
SIEVE:=src/mailkat.native
EXAMPLE_OUTPUT = ./examples_output

all:
	$(OCB) src/mailkat.native

tests: all sieve-tests

%.native:
	$(OCB) "$@"

clean: clean-tests
	$(OCB) -clean

clean-tests:
	rm -rf $(EXAMPLE_OUTPUT) ;
	rm -f *.sieve

# Will not work with Windows.
%.txt: FORCE
	mkdir -p $(EXAMPLE_OUTPUT)/$(notdir $(basename $@)) ;
	_build/$(SIEVE) -o $(EXAMPLE_OUTPUT)/$(notdir $(basename $@))/ -to-sieve $@;

sieve-tests:
	-@for TST in $(EXAMPLEDIR)/*.txt ; do $(MAKE) $$TST ; done

FORCE: ;

# MailKAT Compiler

MailKAT compiler implementation as described in the complementary report.

### Prerequisites

* OCaml (with ocamlfind and ocamllex) - (tested with 4.06.1)
* Menhir
* Core pkg

### Usage

```
# Make program
make

# Usage
./mailkat.native [-p] -to-sieve <file>

# Clean
make clean
```
where [-p] can be used to output to console.

### Test Programs

To compile the test programs provided in the "examples" folder, run:

```
# Make tests
make sieve-tests

# Clean
make clean-tests
```

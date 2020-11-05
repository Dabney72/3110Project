MODULES=main display input state tetromino
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TETR_TEST = tetromino_test.byte
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

tetr_test:
	$(OCAMLBUILD) -tag 'debug' $(TETR_TEST) && ./$(TETR_TEST)

test_all:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)
	$(OCAMLBUILD) -tag 'debug' $(TETR_TEST) && ./$(TETR_TEST)

start:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip tetris.zip *.ml* *.txt _tags Makefile .merlin .ocamlinit -x _build

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private tetris.zip
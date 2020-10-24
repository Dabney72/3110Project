MODULES=main display input state tetromino
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

start:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip tetris.zip *.ml* *.json _tags Makefile .merlin .ocamlinit -x _build
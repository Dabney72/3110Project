MODULES=main display input state tetromino
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
STATE_TEST = state_test.byte
TETR_TEST = tetromino_test.byte
DISPLAY_TEST = display_test.byte
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

display_test:
	$(OCAMLBUILD) -tag 'debug' $(DISPLAY_TEST) && ./$(DISPLAY_TEST)

tetr_test:
	$(OCAMLBUILD) -tag 'debug' $(TETR_TEST) && ./$(TETR_TEST)
	
state_test:
	$(OCAMLBUILD) -tag 'debug' $(STATE_TEST) && ./$(STATE_TEST)

test:
	$(OCAMLBUILD) -tag 'debug' $(TETR_TEST) && ./$(TETR_TEST)
	$(OCAMLBUILD) -tag 'debug' $(STATE_TEST) && ./$(STATE_TEST)

start:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip tetris.zip *.ml* *.txt _tags Makefile .merlin .ocamlinit -x _build

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private tetris.zip
MODULES=main display state tetromino authors
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
STATE_TEST = state_test.byte
TETR_TEST = tetromino_test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)'
PKGS=ounit2,graphics

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

tetr_test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TETR_TEST) && ./$(TETR_TEST) -runner sequential
	
state_test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(STATE_TEST) && ./$(STATE_TEST) -runner sequential

test: tetr_test state_test

bisect: clean test
	bisect-ppx-report html

start:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip tetris.zip *.ml* *.txt _tags Makefile .merlin .ocamlinit -x _build

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf tetris.zip doc.public doc.private _coverage bisect*.coverage
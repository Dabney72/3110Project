MODULES=main display state tetromino authors strategy strategies move \
printers
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST = test.byte
STATE_TEST = state_test.byte
TETR_TEST = tetromino_test.byte
MOVE_TEST = move_test.byte
STRATEGY_TEST = strategy_test.byte
STRATEGIES_TEST = strategies_test.byte
MAIN=main.byte
TRAIN=strategies.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)'
PKGS=unix,ounit2,graphics

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

tetr_test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TETR_TEST) && ./$(TETR_TEST) -runner sequential
	
state_test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(STATE_TEST) && ./$(STATE_TEST) -runner sequential

move_test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(MOVE_TEST) && ./$(MOVE_TEST) -runner sequential

strategy_test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(STRATEGY_TEST) && ./$(STRATEGY_TEST) -runner sequential

strategies_test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(STRATEGIES_TEST) && ./$(STRATEGIES_TEST) -runner sequential

test: 
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

bisect: clean test
	bisect-ppx-report html

start:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

train:
	$(OCAMLBUILD) $(TRAIN) && ./$(TRAIN)

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
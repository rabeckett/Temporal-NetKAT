MLFILES= src/common.ml src/bag.ml src/bag.mli src/input.ml src/input.mli src/ptmap.ml src/ptmap.mli src/automata.ml src/automata.mli src/profile.ml src/profile.mli src/tkat.ml src/syntax.ml src/syntax.mli src/fdd.ml src/fdd.mli src/lexer.mll src/parser.mly \

all: tkat.native

tkat.native: $(MLFILES)
	ocamlbuild -use-ocamlfind -r src/tkat.native

test:
	./tkat.native --test

pldi-small:
	python scripts/pldi-experiments.py stanford

pldi:
	python scripts/pldi-experiments.py all
	python scripts/add_nodes.py
	python scripts/graphs.py

clean:
	ocamlbuild -clean
	rm -f bin/*.native

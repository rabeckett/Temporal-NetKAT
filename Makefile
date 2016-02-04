MLFILES= src/common.ml src/input.ml src/input.mli src/ptmap.ml src/ptmap.mli src/automata.ml src/automata.mli src/profile.ml src/tkat.ml src/syntax.ml src/syntax.mli src/fdd.ml src/fdd.mli src/lexer.mll src/parser.mly \

all: tkat.native

tkat.native: $(MLFILES)
	ocamlbuild -use-ocamlfind -r src/tkat.native

test:
	./tkat.native --test

pldi-small:
	python scripts/pldi-experiments.py stanford

pldi:
	python scripts/pldi-experiments.py all
	python scripts/graph.py

clean:
	ocamlbuild -clean
	rm -f bin/*.native

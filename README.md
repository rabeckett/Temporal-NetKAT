# Building

To build the code from source, you will need the Ocaml compiler (4.02.1 or higher) and the ocamlfind and ppx_deriving utilities. The easiest way to install these is via opam:

```
opam install ocamlfind ppx_deriving
```

Then run `make` to build the source. The main exectuable will be: `tkat.native`


# Usage

Usage: tkat [options]
  * \-\-in   Input file name (default stdin)
  * \-\-out   Output file name (default none)
  * \-\-test   Runs unit tests
  * \-\-stats   Output performance statistics as csv to stdout
  * \-\-help  Display this list of options

`echo "sw<-A;dup;last(sw=A)" | ./tkat.native --out rules.txt`

# Experiments

To run the compiler on examples from the Topology Zoo and Stanford network requires Python (2.7), and the [networkx](https://networkx.github.io/) library. Then do the following:

```
cd scripts/
python compile.py
```

Note: This may take a very long time to complete.

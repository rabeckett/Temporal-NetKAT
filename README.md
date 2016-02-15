# Building

### Virtual Machine
The easiest way to use the compiler is to download the Ubuntu virtual [image](http://www.cs.princeton.edu/~rbeckett/Temporal-NetKAT.ova) with the compiler already set up. 

If it asks for a username and password, the username is "ubuntu", and the password is "password". The main compiler directory can be found in

```/home/ubuntu/Desktop/Temporal-NetKAT```

### From Source
To build the code from source, you will need the [Ocaml](https://ocaml.org/) compiler (>= 4.02.1) and the ocamlfind and ppx_deriving utilities. The easiest way to install everything is via [opam](https://opam.ocaml.org/):

```
opam install ocamlfind ppx_deriving
```

Then run `make` to build the source. The main exectuable will be: `tkat.native`

To run the experiments from the paper, you will need 
* [python (>=2.7)](https://www.python.org/)
* [matplotlib](http://matplotlib.org/) 
* [networkx](https://networkx.github.io/)


# Usage
To test that the compiler is working, you can run its unit and randomized tests by running the main executable `tkat.native` in the `Temporal-NetKAT` directory:

`./tkat.native -test`

To compile a simple example policy, we can run the following command:

`./tkat.native -in examples/path_monitor.tkat -out rules.txt`

Other policy examples are contained in the `examples/` directory.

```
Usage: tkat [options]
  * \-in   Input file name (default stdin)
  * \-out   Output file name (default none)
  * \-test   Runs unit tests
  * \-stats   Output performance statistics as csv to stdout
  * \-help  Display this list of options
```

# Experiments

To test the compiler quickly, you can compile just the stanford network with:

`make pldi-small`

This should take no more than a minute or two. To compile all policies for both the Stanford network and the Topology Zoo, run, and to reproduce the graphs used in the paper, run the command:

`make pldi`

Compilation was tested on a MacBook Pro with an 8-core, 2.4 GHz Intel Core i7 processor with 8GB RAM and took just under 2 hours to complete. The output compiler performance statistics, Temporal NetKAT policy files, and generated graphs will be in the scripts/output directory.
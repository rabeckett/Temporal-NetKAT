# Temporal NetKAT

This is the compiler for the Temporal NetKAT language described in the PLDI submission [here](http://www.cs.princeton.edu/~rbeckett/Temporal-NetKAT.pdf).

# Building

### Virtual Machine
The easiest way to use the compiler is to download the Ubuntu virtual [image](http://www.cs.princeton.edu/~rbeckett/Temporal-NetKAT.ova) with the compiler already set up. 

If it asks for a username and password, the username is "ubuntu", and the password is "password". The main compiler directory can be found in

`/home/ubuntu/Desktop/Temporal-NetKAT`

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

If there is no output for the tests, then run `make clean` followed by `make` to rebuild the executable.
To compile a simple example policy, we can run the following command:

`./tkat.native -in examples/path_monitor.tkat -out rules.txt`

An explanation of each example is given in the comments of the .tkat file. The result is a list of prioritized Openflow-like rules, that match and modify packet fields. Running `cat rules.txt` displays the resulting rules:

```
[sw=A,pt=1]          ==>  {{state<-4,pt<-2}}
[sw=A,pt=2,state=4]  ==>  {{pt<-1}}
[sw=A,pt=2,state=5]  ==>  {{pt<-bucket_1},{pt<-1}}
[sw=B,pt=1]          ==>  {{state<-5,pt<-2}}
[sw=B,pt=2,state=4]  ==>  {{pt<-bucket_2},{pt<-1}}
[sw=B,pt=2,state=5]  ==>  {{pt<-1}}
[]                   ==>  drop
```

For example, a packet entering the network at switch A and port 1 will be moved to port 2, and have its state (VLAN tag) updated to indicate it is in automaton state 4. If the packet ends up at port 2 on switch B, the packet will then be multicast with one copy of the packet going to port 1 on switch B, and the other going to a controller bucket for counting monitoring statistics.

Other policy examples are contained in the `examples/` directory. Compiler usage information is listed below.

```
Usage: tkat [options]
  -in     Input file name (default stdin)
  -out    Output file name (default none)
  -test   Runs unit tests
  -stats  Output performance statistics as csv to stdout
  -help   Display this list of options
```

# Experiments

### Data

All of the data for the performance benchmarks is contained in the `data/` directory. The topology zoo data can be found in the `data/zoo` directory. Each topology is encoded in the .gml graph language format. The stanford data can be found in the `data/stanford` directory. The `port_map.txt` and `backbone_topology.tf` files describe the backbone topology, while the .of files describe Openflow rules in a json format.

After running the experiments, the generated Temporal NetKAT files (.tkat) can be found in the `scripts/zoo` and `scripts/stanford` directories.

### Running

To test the compiler quickly, you can compile just the stanford network with:

`make pldi-small`

This should take no more than a minute or two. To compile all policies for both the Stanford network and the Topology Zoo, run, and to reproduce the graphs used in the paper, run the command:

`make pldi`

Compilation was tested on a MacBook Pro with an 8-core, 2.4 GHz Intel Core i7 processor with 8GB RAM and took just under 2 hours to complete. The output compiler performance statistics, Temporal NetKAT policy files, and generated graphs will be in the `scripts` directory.
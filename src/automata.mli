type t
(** NetKAT automaton type *)

val create: Syntax.term -> t
(** Create an automaton from a regular NetKAT term *)

val determinize: t -> t
(** Determinize the automaton using the powerset construction *)

val compile: t -> Syntax.updates Fdd.fdd
(** Extract the local components from an automaton *)

val minimize: t -> t
(** Heuristic automata minimization *)

val instrument: Syntax.tterm -> t * int
(** Compile and instrument a policy with query monitoring  *)

val print: t -> unit 
(** Print the automata in human-readable form *)

module Test: sig
	val unit_tests: unit -> unit
end


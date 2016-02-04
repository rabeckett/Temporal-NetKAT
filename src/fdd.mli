open Common
open Syntax

type 'a fdd
(** Forwarding Decision Diagram over polymorphic leaf type *)

type 'a mem
(** An FDD memory to ensure unicity of nodes, 
   functions require a memory from which to perform hashconsing. 
   Memories should be global and reused to ensure correctness *) 

type rules = ((field_val list) * updates) list
(** Forwarding rules contain match * action pairs *)


module FddType (X: sig type t end) : (Set.OrderedType with type t = X.t fdd)
(** Module for bulding efficient sets/maps over fdds *)

module FddType2 (X: sig type t end) (Y: sig type t end) 
  : (Set.OrderedType with type t = X.t fdd * Y.t fdd)
(** Module for bulding efficient sets/maps over fdds *)


val create_mem: ('a -> int) -> ('a -> 'a -> bool) -> 'a mem
(** Create a memory with a equality and hash function *)

val node: 'a mem -> field_val -> 'a fdd -> 'a fdd -> 'a fdd
(** Build a new fdd node from old nodes and a field + value *)

val value: 'a mem -> 'a -> 'a fdd
(** Build a leaf fdd node from a value of type 'a *)

val apply: 'b mem -> ('a -> 'a -> 'b) -> 'a fdd -> 'a fdd -> 'b fdd
(** Standard apply operator over an fdd. Base element type can change *)

val map: 'b mem -> ('a -> 'b) -> 'a fdd -> 'b fdd
(** Map over elements of an fdd. Base element type can change *)

val map_path: 'b mem -> (Syntax.update -> 'a -> 'b) -> 'a fdd -> 'b fdd
(** Map over elements of an fdd, with field-value sequence of the path provided *)

val elements: 'a fdd -> 'a Bag.t
(** Return all of the leaf elements of the fdd as a bag *)

val replace: 'a mem -> field_val -> 'a fdd -> updates fdd  -> 'a fdd
(** Performs substitution for a field-value with an expression (updates fdd) *)

val eval: 'a fdd -> Syntax.update -> 'a
(** Evaluate an fdd given concrete values for fields and corresponding values *)

val show_fdd: ('a -> string) -> 'a fdd -> string 
(** Get a string representation for an fdd *)


val updates_mem: updates mem
(** Specialized memory for fdds where leaves are specialized to NetKAT modifications *)

val bot: updates fdd
(** Fdd representing false *)

val top: updates fdd
(** Fdd representing true *)

val var: field_val -> updates fdd
(** Fdd for a test, i.e., f = v *)

val assign: field_val -> updates fdd
(** Fdd for an assignment, i.e., f <- v *)

val plus: updates fdd -> updates fdd -> updates fdd
(** Sum of two fdds representing NetKAT (+) operator *)

val seq: updates fdd -> updates fdd -> updates fdd
(** Sequence of two fdds representing NetKAT (;) operator *)

val neg: updates fdd -> updates fdd
(** Negation of an fdd representing NetKAT not operator *)

val star: updates fdd -> updates fdd
(** Kleene Star fixpoint of an fdd *)

val create: Syntax.term -> updates fdd
(** Create an fdd from a NetKAT term *)


val rules: updates fdd -> rules * rules * rules * int
(** Generate forwarding rules from an fdd (return multiple values for stats) *)

val write_rules: out_channel -> rules -> unit
(** Write rules to an out channel for simplicity *)


module Test: sig
	val unit_tests: unit -> unit 
end

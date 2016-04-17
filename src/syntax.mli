open Common
open Int32

type field_val = 
	| Sw of string 
	| Pt of string 
	| Src of int32 * int
	| Dst of int32 * int
	| State of int
	| Placeholder of int
	[@@deriving eq, ord]
(** Field and corresponding value *)

type value_comparison =
	| Equal
	| Less
	| Greater
	| LeftSubsumes
	| RightSubsumes
(* comparison result for values.
   generalized to handle ip prefixes *)

val get_value: field_val -> string
val to_field_val: string * string * string option -> field_val
val show_field_val: string -> field_val -> string
val show_field: field_val -> string 
val show_val: field_val -> string
val compare_field: field_val -> field_val -> int 
val compare_val: field_val -> field_val -> value_comparison
val hash_fv: field_val -> int
(** Helper functions for hashing/printing/comparing *)

type tterm = 
	| Tzero
	| Tone 
	| Ttest of field_val
	| Tassign of field_val
	| Tneg of tterm
	| Tand of tterm * tterm 
	| Tor of tterm * tterm
	| Tseq of tterm * tterm
	| Tplus of tterm * tterm
	| Tlast of tterm
	| Twlast of tterm
	| Tever of tterm 
	| Talways of tterm 
	| Tstart
	| Tstar of tterm
	| Tdup
	[@@deriving eq, ord]
(** Temporal NetKAT term *)

type term =
	| Zero 
	| One
	| Dup of int 
	| Test of field_val
	| Assign of field_val 
	| Seq of term * term
	| Sum of term * term
	| Star of term
	| Neg of term
	[@@deriving eq, ord]
(** NetKAT term *)

val zero: term
val one: term 
val dup: unit -> term 
val test: field_val -> term 
val assign: field_val -> term
val seq: term -> term -> term
val sum: term -> term -> term
val star: term -> term
val neg: term -> term
(** Smart constructors *)

val is_local: term -> bool
(** Check if a policy is local *)

val unique_tag: unit -> int
(** Generate a unique integer tag *)

val extract_queries: tterm -> term * (tterm NatMap.t)
(** make a Temporal NetKAT term history free by extracting queries *)

val show_term: term -> string 
val show_tterm: tterm -> string

val size_term: term -> int 
val size_tterm: tterm -> int 
val size_breakdown: tterm -> int * int
(** AST term size *)

val replace_placeholders: term -> term NatMap.t -> term 
(** Performs substitution of the placeholders for the concrete terms *)

module Update: Set.S with type elt = field_val
module Updates: Set.S with type elt = Update.t
type update = Update.t
type updates = Updates.t
(** Packet modifications *)

val merge_right: update -> field_val -> update
val merge_right_all: update -> update -> update
(** Right-biased merge of updates *)

val show_update: update -> string
val hash_update: update -> int 
val show_updates: updates -> string
val hash_updates: updates -> int
(** Helper functions for hashing/printing *)

val map_update: (field_val -> field_val) -> update -> update
val map_updates: (update -> update) -> updates -> updates 
(** Convenience functions for manipulating updates *)

(* Generate random local/global policies *)
module Arbitrary: sig 
	val gen_term_local: unit -> term
	val gen_tterm: unit -> tterm
end

(* Unit tests *)
module Test: sig 
	val unit_tests: unit -> unit
end
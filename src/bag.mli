type 'a t
(** Polymorphic bag of elements *)

val empty: 'a t
(** The empty bag *)

val singleton: 'a -> 'a t
(** Bag containing a single element *)

val append: 'a t -> 'a t -> 'a t
(** Append two bags together, O(1) operation *)

val add: 'a -> 'a t -> 'a t
(** Add an element to a bag O(1) operation *)

val map: ('a -> 'b) -> 'a t -> 'b t
(** Map a function over a bag *)

val fold: ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b 
(** Left fold over a bag *)

val iter: ('a -> unit) -> 'a t -> unit
(** Iterate a function over elements of a bag *)

val length: 'a t -> int
(** Length of the bag *)

val first: 'a t -> 'a
(** Get the first element of a bag *)

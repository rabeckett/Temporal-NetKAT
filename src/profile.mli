val time_verbose: string -> ('a -> 'b) -> 'a -> 'b
(** Time a function, print out the time with a description, return the value *)

val time: ('a -> 'b) -> 'a -> 'b * float 
(** Time a function and return the result and the time it took in seconds *)

val time_map : (string, float) Hashtbl.t
(** A map that tallies total time for functions run under a given description *)

val profile: string -> ('a -> 'b) -> 'a -> 'b
(** Adds the time for a function under a given description, and returns the value *)

val print_times: unit -> unit
(** Print the total time for each description used in the profile function *)
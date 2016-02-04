(*******************************************************************)
(*                                                                 *)
(*                 Simple parsing from input                       *)
(*                                                                 *)
(*******************************************************************)

open Syntax

val read_from_in: in_channel -> Syntax.tterm 
(** Read a Temporal NetKAT term from an input stream *)

val read_from_str: string -> Syntax.tterm
(** Read a Temporal NetKAT term from a string *)

val read_from_file: string -> Syntax.tterm
(** Read a Temporal NetKAT term from a file *)

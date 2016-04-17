(*******************************************************************)
(*                                                                 *)
(*         A collection of commonly useful functions               *)
(*                                                                 *)
(*******************************************************************)

open Bag

(* Helper functions for dealing with 
  the standard library option type *)

module Option = struct 

  let unwrap o = 
    match o with 
    | None -> failwith "unwrap"
    | Some v -> v

  let is_none o = 
    match o with 
    | None -> true 
    | Some _ -> false

  let is_some o = 
    match o with 
    | None -> false 
    | Some _ -> true

  let get o = 
    match o with 
    | None -> failwith "[Option.get]: None value"
    | Some v -> v
end

let unreachable () = 
  failwith "unreachable"

(* Convenience functions that help for 
   debugging various collection types *)

let add_sep sep acc = 
  if acc = "" then acc else sep ^ acc

let show_bag f bag = 
  let elts = Bag.fold (fun x acc -> (f x) ^ (add_sep "," acc)) "" bag in 
  "[" ^ elts ^ "]"

let show_set f fold set = 
  let elts = fold (fun x acc -> (f x) ^ (add_sep "," acc)) set "" in 
  "{" ^ elts ^ "}"

let show_list f lst = 
  let elts = List.fold_left (fun acc x -> (f x) ^ (add_sep "," acc)) "" lst  in 
  "[" ^ elts ^ "]"

let show_map fkey fval fold map = 
  let aux k v acc = (fkey k) ^ "==>" ^ (fval v) ^ (add_sep "," acc) in 
  "{" ^ (fold aux map "") ^ "}"

(* Set default seed value to make  
   randomized tests deterministic *)

let _ = Random.init 17

let _hash x acc = 
  (acc lsr 5) - 1 + x

(* Specialize Maps and Sets for commonly used 
   int and string types. Provides more efficient 
   comparison/hash/equality functions than using 
   polymorphic compare. Since natural numbers are used
   often, we use subtraction for comparison without 
   worrying about overflow. *)

module IntType = 
  struct 
    type t = int 
    let equal i j = i=j
    let compare (i:int) j = 
      if i<j then -1 
      else if i > j then 1 
      else 0
    let hash i = i land max_int
  end

module IntType2 = 
  struct 
    type t = int*int
    let equal x y = compare x y = 0
    let compare (a,b) (c,d) = 
      let cmp = IntType.compare a c in 
      if cmp = 0 then IntType.compare b d else cmp
    let hash (i,j) = (i land max_int) + (j land max_int)
  end

module NatType = 
  struct 
    type t = int 
    let compare i j = i-j
    let equal i j = i=j
    let hash i = i land max_int
  end

module NatType2 = 
  struct
    type t = int*int 
    let equal x y = compare x y = 0
    let compare (a,b) (c,d) = 
      let cmp = a - c in
      if cmp = 0 then b-d else cmp   
    let hash (i,j) = (i + j) land max_int
  end

module StrType = 
  struct 
    type t = string 
    let compare = String.compare
    let equal i j = String.compare i j = 0
    let hash s = 
      let h = ref 0 in 
      for i = 0 to String.length s - 1 do 
        h := (!h lsr 5) - 1 + (Char.code s.[i])
      done; 
      !h
  end

module NatSet = struct 
  module S = Set.Make(NatType)
  include S
  let hash x = 
    S.fold (fun y acc -> _hash y acc) x 0
end

module StrSet = Set.Make(StrType)
module StrMap = Map.Make(StrType)
module NatMap = Map.Make(NatType)









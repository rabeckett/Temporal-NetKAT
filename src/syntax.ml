(*******************************************************************)
(*                                                                 *)
(*                 Temporal NetKAT syntax terms        			   *)
(*                                                                 *)
(*******************************************************************)

open Common
open Random 

exception NoSuchField

(* Order of fields is important, as it determines the structure of fdds.
   More common fields appear higher up, and the Placeholder must appear 
   at the end to make substitution significantly simpler. *)
type field_val = 
	| Sw of string 
	| Pt of string 
	| Src of string 
	| Dst of string 
	| State of string
	| Placeholder of int
	[@@deriving eq, ord]

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

let unique_tag =
    let r = ref 0 in
    fun () -> incr r; !r

let to_field_val (f,v) = 
	match f with 
	| "state" -> State v 
	| "sw" -> Sw v 
	| "pt" -> Pt v 
	| "src" -> Src v 
	| "dst" -> Dst v
	| _ -> raise NoSuchField

let show_field_val sep fv = 
	match fv with 
	| State v -> "state" ^ sep ^ v
	| Sw v -> "sw" ^ sep ^ v
	| Pt v -> "pt" ^ sep ^ v
	| Src v -> "src" ^ sep ^ v
	| Dst v -> "dst" ^ sep ^ v
	| Placeholder i -> "$" ^ (string_of_int i) 

let show_field fv = 
	match fv with 
	| State _ -> "state"
	| Sw _ -> "sw"
	| Pt _ -> "pt"
	| Src _ -> "src"
	| Dst _ -> "dst"
	| Placeholder i -> "$" ^ (string_of_int i)

let show_val fv = 
	match fv with 
	| State v -> v
	| Sw v -> v
	| Pt v -> v
	| Src v -> v
	| Dst v -> v
	| Placeholder v -> "$"

let field_int fv = 
	match fv with 
	| Sw _ -> 1
	| Pt _ -> 2
	| Src _ -> 3
	| Dst _ -> 4
	| State _ -> 5
	| Placeholder i -> 6 + i

let compare_field fv1 fv2 = 
	(field_int fv1) - (field_int fv2)

let compare_val fv1 fv2 =
	match fv1, fv2 with 
	| Placeholder i1, Placeholder i2 -> 0 
	| _, _ -> StrType.compare (show_val fv1) (show_val fv2)

let hash_fv fv = 
	match fv with 
	| State v -> 2 + StrType.hash v
	| Sw v -> 3 + StrType.hash v
	| Pt v -> 5 + StrType.hash v
	| Src v -> 7 + StrType.hash v
	| Dst v -> 11 + StrType.hash v
	| Placeholder v -> 13 + IntType.hash v

let zero = Zero
let one = One 
let dup () = Dup (unique_tag ())
let test fv = Test fv
let assign fv = Assign fv

let seq a b = 
	match a,b with 
	| Zero, _ | _, Zero -> Zero 
	| One, _ -> b 
	| _, One -> a 
	| _, _ -> Seq (a,b)

let sum a b = 
	match a,b with 
	| Zero, _ -> b 
	| _, Zero -> a
	| _, _ -> Sum (a,b)

let star a = 
	match a with 
	| Test _ -> One
	| Star _ -> a
	| _ -> Star a

let neg a =
	match a with 
	| Neg b -> b 
	| _ -> Neg a

let rec is_local (t: term) : bool = 
	match t with 
	| Zero | One | Test _ | Assign _ -> true
	| Dup _ -> false
	| Seq(a,b) | Sum(a,b) -> is_local a && is_local b
	| Star a | Neg a -> is_local a

module Cache = Map.Make(struct type t = tterm let compare = compare_tterm end)

let rec extract_queries tt =
	let map = ref NatMap.empty in 
	let cache = ref Cache.empty in
	let rec aux tt = 
		match tt with 
		| Tone -> one
		| Tzero -> zero
		| Ttest fv -> test fv
		| Tassign fv -> assign fv
		| Tdup -> dup ()
		| Tand (a,b) | Tseq (a,b) -> seq (aux a) (aux b)
		| Tor (a,b) | Tplus (a,b) -> sum (aux a) (aux b) 
		| Tneg a -> neg (aux a) 
		| Tstar a -> star(aux a) 
		| _ ->
			try 
				let idx = Cache.find tt !cache in 
				test (Placeholder idx)
			with Not_found -> 
				let id = unique_tag () in 
				map := NatMap.add id tt !map;
				cache := Cache.add tt id !cache;
				test (Placeholder id)
	in 
	let trm = aux tt in 
	(trm, !map)

let rec show_term t = 
	match t with 
	| Zero -> "drop"
	| One -> "id"
	| Dup id -> "dup(" ^ (string_of_int id) ^ ")"
	| Test fv -> show_field_val "=" fv
	| Assign fv -> show_field_val "<-" fv
	| Seq (a,b) -> (show_term a) ^ ";" ^ (show_term b)
	| Sum (a,b) -> "(" ^ (show_term a) ^ " + " ^ (show_term b) ^ ")"
	| Star p -> "(" ^ (show_term p) ^ ")*"
	| Neg a -> "-(" ^ (show_term a) ^ ")"

let rec show_tterm t = 
	match t with 
	| Tzero -> "drop"
	| Tone -> "id"
	| Tdup -> "dup"
	| Ttest fv ->  show_field_val "=" fv
	| Tassign fv -> show_field_val "<-" fv
	| Tseq(a,b) -> (show_tterm a) ^ ";" ^ (show_tterm b)
	| Tand(a,b) -> (show_tterm a) ^ " and " ^ (show_tterm b)
	| Tplus(a,b) -> "(" ^ (show_tterm a) ^ " + " ^ (show_tterm b) ^ ")"
	| Tor(a,b) -> "(" ^ (show_tterm a) ^ " or " ^ (show_tterm b) ^ ")"
	| Tstar p -> "(" ^ (show_tterm p) ^ ")*"
	| Tneg a -> "-(" ^ (show_tterm a) ^ ")"
	| Tlast a -> "last(" ^ (show_tterm a) ^ ")"
	| Twlast a -> "wlast(" ^ (show_tterm a) ^ ")"
	| Tever a -> "ever(" ^ (show_tterm a) ^ ")"
	| Talways a -> "always(" ^ (show_tterm a) ^ ")"
	| Tstart -> "start"

let rec size_term term = 
	match term with 
	| Zero | One | Dup _ -> 1
	| Test _ | Assign _ -> 1
	| Seq (l,r) | Sum (l,r) -> 1 + (size_term l) + (size_term r)
	| Star t | Neg t -> 1 + (size_term t)

let rec size_tterm tterm = 
	match tterm with
	| Tzero | Tone | Tdup | Tstart -> 1
	| Ttest _ | Tassign _ -> 1
	| Tneg t | Tstar t | Tlast t | Twlast t | Tever t | Talways t ->
		1 + (size_tterm t)
	| Tand (l,r) | Tor (l,r) | Tseq (l,r) | Tplus (l,r) -> 
		1 + (size_tterm l) + (size_tterm r)

let size_breakdown tterm = 
	let term, qrys = extract_queries tterm in 
	let sz_term = size_term term in 
	let sz_qrys = NatMap.fold (fun _ v acc -> acc + (size_tterm v)) qrys 0 in 
	sz_term, sz_qrys

let rec replace_placeholders t map = 
	match t with 
	| Zero | One | Dup _ -> t
	| Test (Placeholder i) -> NatMap.find i map
	| Test _ | Assign _ -> t
	| Seq(l,r) -> Seq (replace_placeholders l map, replace_placeholders r map)
	| Sum(l,r) -> Sum (replace_placeholders l map, replace_placeholders r map)
	| Star x -> Star (replace_placeholders x map)
	| Neg x -> Neg (replace_placeholders x map)

module Update = Set.Make(struct 
	type t = field_val 
	let compare = compare_field_val
end)

type update = Update.t

let show_update u = 
	if Update.is_empty u then "id"
	else show_set (show_field_val "<-") Update.fold	u

let hash_update u =
	Update.fold (fun fv acc -> (hash_fv fv) + acc) u 0

let map_update f u = 
	Update.fold (fun fv acc -> Update.add (f fv) acc) u Update.empty

module Updates = Set.Make(struct 
	type t = update
	let compare = Update.compare
end)

type updates = Updates.t

let show_updates us = 
	if Updates.is_empty us then "drop" 
	else 
		let res = show_set show_update Updates.fold us in 
		if res = "{id}" then "id" else res

let hash_updates us = 
	Updates.fold (fun u acc -> (hash_update u) + acc) us 0

let map_updates f us = 
	Updates.fold (fun u acc -> Updates.add (f u) acc) us Updates.empty

let merge_right u fv =
	Update.filter (fun fv' -> compare_field fv' fv <> 0) u
	|> Update.add fv

let rec merge_right_all u1 u2 =
	Update.fold (fun fv acc -> merge_right acc fv) u2 u1


module Arbitrary = struct 
	let max_depth_local = 5
	let max_depth_global = 3

	let gen_fv () = 
		match Random.int 9 with 
		| 0 -> Sw "A"
		| 1 -> Sw "B"
		| 2 -> Sw "C"
		| 3 -> Pt "1"
		| 4 -> Pt "2"
		| 5 -> Pt "3"
		| 6 -> Dst "10.0.0.1"
		| 7 -> Dst "10.0.0.2"
		| _ -> Dst "10.0.0.3"

	let gen_term_local () = 
		let rec aux d = 
			if d = 0 then one 
			else
				(let rand = Random.int 20 in 
				if rand >= 17 then test (gen_fv ())
				else if rand >= 14 then assign (gen_fv ())
				else if rand >= 13 then zero 
				else if rand >= 12 then one 
				else if rand >= 8 then seq (aux (d-1)) (aux (d-1))
				else if rand >= 4 then sum (aux (d-1)) (aux (d-1))
				else if rand >= 2 then star (aux (d-1))
				else neg (aux (d-1)) )
		in aux max_depth_local

	let gen_ttest () = 
		let rec aux d = 
			if d = 0 then Tone 
			else
				(let rand = Random.int 100 in 
				if rand >= 95 then Tzero 
				else if rand >= 90 then Tone 
				else if rand >= 70 then Ttest (gen_fv ())
				else if rand >= 60 then Tneg (aux (d-1))
				else if rand >= 50 then Tand (aux (d-1), aux (d-1)) 
				else if rand >= 40 then Tor (aux (d-1), aux (d-1)) 
				else if rand >= 35 then Twlast (aux (d-1))
				else if rand >= 30 then Tlast (aux (d-1))
				else if rand >= 16 then Tever (aux (d-1))
				else if rand >= 3 then Talways (aux (d-1))
				else Tstart)
		in aux max_depth_global

	let gen_tterm () = 
		let rec aux d = 
			if d = 0 then Tone 
			else
				(let rand = Random.int 100 in 
				if rand >= 80 then Tassign (gen_fv ())
				else if rand >= 60 then gen_ttest ()
				else if rand >= 40 then Tseq (aux (d-1), aux (d-1)) 
				else if rand >= 20 then Tplus (aux (d-1), aux (d-1)) 
				else if rand >= 10 then Tstar (aux (d-1))
				else Tdup)
		in aux max_depth_global
end
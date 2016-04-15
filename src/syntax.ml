open Common
open Int32


exception NoSuchField

(* Order of fields is important, as it determines the structure of fdds.
   More common fields appear higher up, and the Placeholder must appear
   at the end to make substitution significantly simpler.
   If you change this order, change the compare_field function *)
type field_val =
	| Sw of string
	| Pt of int
	| Src of int32 * int
	| Dst of int32 * int
	| State of int
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
    (fun () -> incr r; !r)

let int_from_dotted_ip (w,x,y,z) =
	let in_range i = (i >= 0 && i < 256) in 
	if not (in_range w && in_range x && in_range y && in_range z) then 
		failwith "ip out of range 0-255"
 	else
		let a = Int32.shift_left (Int32.of_int w) 24 in 
		let b = Int32.shift_left (Int32.of_int x) 16 in 
		let c = Int32.shift_left (Int32.of_int y) 8 in
		Int32.logor a (Int32.logor b (Int32.logor c (Int32.of_int z)))

let get_first_n_bits x bits =
	if bits = 0 then Int32.zero 
	else
		let shift = 32 - bits in 
		let i = Int32.shift_right_logical x shift in 
		Int32.shift_left i shift

let longer_prefix_of (x,xbits) (y,ybits) = 
	(xbits >= ybits) && 
	(compare (get_first_n_bits x ybits) (get_first_n_bits y ybits) = 0)

let dot_regex = Str.regexp ("\\.")

let get_prefix v o_pfx = 
	let dots = Str.split dot_regex v in 
	let v = 
		match dots with 
		| [hd] -> Int32.of_string hd 
		| w::x::y::z::[] ->
				let w = int_of_string w in 
				let x = int_of_string x in 
				let y = int_of_string y in 
				let z = int_of_string z in 
				int_from_dotted_ip (w, x, y, z)
		| _ -> failwith ("invalid value: " ^ v) in  		
	match o_pfx with 
	| None -> (v, 32)
	| Some p ->
	    let i = int_of_string p in 
		  if (i < 0 || i > 32) then failwith "invalid prefix"
		  else (get_first_n_bits v i, i) 

let to_field_val (f,v,o_pfx) =
	match f with
	| "state" -> State (int_of_string v)
	| "sw" -> Sw v
	| "pt" -> Pt (int_of_string v)
	| "src" -> 
			let (i,bits) = get_prefix v o_pfx in 
			Src (i,bits)
	| "dst" ->
			let (i,bits) = get_prefix v o_pfx in 
			Dst (i,bits)
	| _ -> raise NoSuchField

let string_of_prefix i bits = 
  let x = Int32.to_string i in 
  if bits = 32 then x 
  else x ^ "/" ^ (string_of_int bits)

let show_field_val sep fv =
	match fv with
	| State v -> "state" ^ sep ^ (string_of_int v)
	| Sw v -> "sw" ^ sep ^ v
	| Pt v -> "pt" ^ sep ^ (string_of_int v)
	| Src (v,bits) -> "src" ^ sep ^ (string_of_prefix v bits)
	| Dst (v,bits) -> "dst" ^ sep ^ (string_of_prefix v bits)
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
	| Sw v -> v
	| Pt v | State v -> (string_of_int v)
	| Src (v,bits) | Dst (v,bits) -> (string_of_prefix v bits)
	| Placeholder v -> "$"

let field_int fv =
	match fv with
	| Sw _ -> 1
	| Pt _ -> 2
	| Src _ -> 3
	| Dst _ -> 4
	| State _ -> 5
	| Placeholder i -> 6 + i

let get_value fv =
	match fv with
	| Sw v -> v
	| State v 
	| Pt v -> string_of_int v
	| Src (v,bits) | Dst (v,bits) -> string_of_prefix v bits
	| Placeholder _ -> failwith "unreachable"

let compare_field fv1 fv2 =
	(field_int fv1) - (field_int fv2)

type value_comparison =
	| Equal
	| Less
	| Greater
	| LeftSubsumes
	| RightSubsumes

let int_to_cmp i = 
	if i < 0 then Less
    else if i > 0 then Greater
    else Equal 

let compare_val fv1 fv2 =
	match fv1, fv2 with
	| State v1, State v2
	| Pt v1, Pt v2 -> int_to_cmp (v1 - v2)
	| Dst (v1,bits1), Dst (v2,bits2)
	| Src (v1,bits1), Src (v2,bits2) -> begin
			let l_subsumed = longer_prefix_of (v1,bits1) (v2,bits2) in 
			let r_subsumed = longer_prefix_of (v2,bits2) (v1,bits1) in 
			match l_subsumed, r_subsumed with 
			| true, true -> Equal 
			| true, false -> RightSubsumes 
			| false, true -> LeftSubsumes 
			| false, false -> 
					let cmp = bits1 - bits2 in 
					if cmp < 0 then Less 
					else if cmp > 0 then Greater 
					else int_to_cmp (Int32.compare v1 v2)
		end
	| Placeholder i1, Placeholder i2 -> Equal
	| Sw v1, Sw v2 -> int_to_cmp (StrType.compare v1 v2)
	| _, _ -> failwith "different fields in compare_val"

let pairing i j =
	let x = i + j in 
	((x * (x+1)) / 2) + j

let hash_fv fv =
	match fv with
	| Sw v -> StrType.hash v
	| Pt v -> pairing 1 v
	| Dst (v,bits) -> pairing 2 (pairing (Int32.to_int v) bits)
	| Src (v,bits) -> pairing 3 (pairing (Int32.to_int v) bits)
	| State v -> pairing 4 v
	| Placeholder v -> pairing 5 v

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


(* Generate random local (no dup) and 
   global terms up to a given depth  *)

module Arbitrary = struct
	let max_depth_local = 5
	let max_depth_global = 3

	let gen_fv () =
		match Random.int 9 with
		| 0 -> Sw "A"
		| 1 -> Sw "B"
		| 2 -> Sw "C"
		| 3 -> Pt 1
		| 4 -> Pt 2
		| 5 -> Pt 3
		| 6 -> Dst (Int32.of_int 1, 32)
		| 7 -> Dst (Int32.of_int 2, 32)
		| _ -> Dst (Int32.of_int 3, 32)

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


(* Unit tests for ip prefix operations
   over 32 bit integers *)

module Test = struct 

	let should_fail f v = 
		try 
			ignore (f v);
			assert false
		with 
			| Assert_failure _ -> assert false 
			| _ -> assert true
	
	let should_succeed f v = 
		(try ignore (f v) with _ -> assert false); 
		assert true 

	let test_parse_ip1 () = should_succeed to_field_val ("src", "10.0.0.1", None)
	let test_parse_ip2 () = should_fail to_field_val ("src", "-1.0.0.1", None)
	let test_parse_ip3 () = should_fail to_field_val ("src", "256.0.0.1", None)
	let test_parse_ip4 () = should_fail to_field_val ("src", "1.0.0.1.2", None)
	let test_parse_ip5 () = should_fail to_field_val ("src", "1.0.0", None)
	
	let test_check_value1 () = 
		for i = 0 to 255 do 
			let x = int_from_dotted_ip (0, 0, 0, i) in
			let y = int_from_dotted_ip (0, 0, i, 0) in 
			let z = int_from_dotted_ip (0, i, 0, 0) in 
			assert (Int32.compare x (Int32.of_int i) = 0);
			assert (Int32.compare x y <= 0);
			assert (Int32.compare y z <= 0)
	  done

	let test_check_value2 () = 
	  let x = int_from_dotted_ip (255, 255, 255, 255) in
		assert (Int32.compare x (Int32.of_int (-1)) = 0)

  let test_ip_comparison1 () = 
		let a = to_field_val ("dst", "0.0.0.0", None) in 
		let b = to_field_val ("dst", "0.0.0.0", Some "32") in 
		assert (compare_val a b = Equal)

  let test_ip_comparison2 () = 
		let a = to_field_val ("dst", "0.0.0.0", None) in 
		let b = to_field_val ("dst", "0.0.0.1", None) in 
		let c = to_field_val ("dst", "0.0.1.0", None) in 
		let d = to_field_val ("dst", "0.1.0.0", None) in 
		let e = to_field_val ("dst", "1.0.0.0", None) in 
		assert (compare_val a b = Less);
		assert (compare_val b c = Less);
		assert (compare_val c d = Less);
		assert (compare_val d e = Less)

  let test_ip_comparison3 () = 
		let a = to_field_val ("dst", "1.2.3.0", Some "24") in 
		let b = to_field_val ("dst", "1.2.3.4", Some "24") in 
		let c = to_field_val ("dst", "1.2.3.4", Some "32") in 
		let d = to_field_val ("dst", "1.2.3.4", Some "16") in 
		assert (compare_val a b = Equal);
		assert (compare_val a c = LeftSubsumes);
		assert (compare_val c a = RightSubsumes);
		assert (compare_val d a = LeftSubsumes);
		assert (compare_val d b = LeftSubsumes)

	let tests = [
		(test_parse_ip1, "parse ip ok");
		(test_parse_ip2, "parse ip negative");
		(test_parse_ip3, "parse ip too large");
		(test_parse_ip4, "parse ip too many params");
		(test_parse_ip5, "parse ip too few params");
		(test_check_value1, "check converted values");
		(test_check_value2, "check converted max value");
		(test_ip_comparison1, "simple ip equality");
		(test_ip_comparison2, "simple ip ordering");
		(test_ip_comparison3, "simple ip subsumption");
	]

	let unit_tests () = 
		print_endline "Testing Prefixes...";
		let rec aux ts = 
			match ts with 
			| [] -> () 
			| (t,descr)::tl -> begin
					(try t () with Assert_failure _ -> print_endline ("  failed: " ^ descr));
					aux tl 
				end
		in aux tests

end
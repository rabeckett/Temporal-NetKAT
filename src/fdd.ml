(*******************************************************************)
(*                                                                 *)
(*         Forwarding Decision Diagram implementation  			       *)
(*         described in: "A fast compiler for NetKAT"              *)
(*                                                                 *)
(*******************************************************************)

open Common
open Syntax
open Hashcons 


let cache_size = 1000

type 'a fdd = ('a node) hash_consed
and 'a node = 
	| Leaf of 'a 
	| Node of field_val * 'a fdd * 'a fdd
type rules = ((field_val list) * updates) list

module FddType (X: sig type t end) : (Set.OrderedType with type t = X.t fdd) = 
  struct 
    type t = X.t fdd 
    let compare i j = i.tag - j.tag
    let equal i j = i.tag = j.tag
    let hash i = i.hkey
  end

module FddType2 (X: sig type t end) (Y: sig type t end) : (Set.OrderedType with type t = X.t fdd * Y.t fdd) = 
  struct 
    type t = X.t fdd * Y.t fdd
    let compare (x1,y1) (x2,y2) =
    	let cmp = x1.tag - x2.tag in 
    	if cmp=0 then y1.tag - y2.tag else cmp 
    let equal x y = (compare x y = 0)
    let hash (i,j) = i.hkey + j.hkey
  end

let peek x = x.node

type 'a mem = {
  table: ('a node) Hashcons.t;
  hash: 'a -> int;
  equal: 'a -> 'a -> bool;
}

let hashcons mem =
	let hash x = 
	  match x with 
	  | Leaf x -> mem.hash x
	  | Node(fv,l,r) -> 
  			abs (hash_fv fv + l.hkey + r.hkey) in
	let equal x y = 
	  match x,y with
	  | Leaf x, Leaf y -> mem.equal x y
	  | Node(fv1,l,r), Node(fv2,l',r') -> 
	    	(compare_field_val fv1 fv2 = 0) && l==l' && r==r'
	  | _ -> false
  in hashcons hash equal mem.table

let create_mem h e = {table=Hashcons.create 57; hash=h; equal=e}

let value mem v = hashcons mem (Leaf v)

let node mem b x y = if x.tag = y.tag then x else hashcons mem (Node(b,x,y))

let rec elements fdd = 
	match peek fdd with 
	| Leaf v -> Bag.singleton v
	| Node(_,l,r) -> Bag.append (elements l) (elements r)

let rec show_fdd f_str fdd = 
	match peek fdd with 
	| Leaf v -> f_str v
	| Node(fv,l,r) -> 
		(show_field_val "=" fv) ^ 
		"? (" ^ (show_fdd f_str r) ^ ") : (" ^ (show_fdd f_str l) ^ ")"

module NP = Hashtbl.Make(NatType2)

let memoize f = 
	let tbl = NP.create (cache_size * 2) in
	let rec aux x y = 
		let k = (x.tag, y.tag) in 
		try NP.find tbl k
		with _ -> 
			let res = f aux x y in 
			if NP.length tbl > cache_size then begin
		    NP.clear tbl
		  end;
			NP.add tbl k res; 
			res in 
	aux

(* restricts an fdd by the field and value known to be true *)
let rec traverse_by_known_value fv x = 
	match peek x with 
	| Leaf _ -> x 
	| Node(fv',l,r) -> 
		if compare_field fv fv' = 0 then
			match compare_val fv fv' with 
			| RightSubsumes -> traverse_by_known_value fv r
			| Less | Greater -> traverse_by_known_value fv l 
			| LeftSubsumes -> x
			| Equal -> failwith "invariant violation"
		else x

(* catch cases where a longer prefix can be 
   combined with a shorter prefix match *)
let roll_prefixes x =
	match x.node with 
	| Node(fv, l, ({node=Node(fvr, lr, rr)} as r)) 
			when (lr = l) && (compare_val fv fvr = RightSubsumes) -> r 
	| Node(fv, ({node=Node(fvr, ll, rl)} as l), r) 
			when (rl = r) && (compare_val fv fvr = RightSubsumes) -> l
	| _ -> x

let rec apply mem f =
	memoize (fun recur x y ->
	match peek x, peek y with
	| Leaf v, Leaf w -> value mem (f v w)
	| Leaf _, Node(b,l,r) -> node mem b (recur x l) (recur x r)
	| Node(b,l,r), Leaf _ -> node mem b (recur l y) (recur r y)
	| Node(fv1,l1,r1), Node(fv2,l2,r2) ->
		  match compare_field fv1 fv2 with 
		  | 0 -> begin
			    match compare_val fv1 fv2 with
			    | Equal -> 
			    		node mem fv1 (recur l1 l2) (recur r1 r2) 
			    | Less ->
			  	   	let z = traverse_by_known_value fv1 l2 in  
			  	   	node mem fv1 (recur l1 y) (recur r1 z)
			    | Greater ->
			  	  	let z = traverse_by_known_value fv2 l1 in  
			  	  	node mem fv2 (recur l2 x) (recur r2 z) 
			  	| LeftSubsumes -> 
 			  	  	let z = traverse_by_known_value fv2 r1 in
 			  	  	roll_prefixes (node mem fv2 (recur l2 x) (recur r2 z))  	  	
			  	| RightSubsumes -> 
 			  	  	let z = traverse_by_known_value fv1 r2 in  
 			  	  	roll_prefixes (node mem fv1 (recur l1 y) (recur r1 z))
			  end
		  | i when i < 0 -> node mem fv1 (recur l1 y) (recur r1 y)
		  | _ -> node mem fv2 (recur l2 x) (recur r2 x) 
	  )

let rec map mem f fdd = 
	match peek fdd with 
	| Leaf v -> value mem (f v)
	| Node(b,l,r) ->
			node mem b (map mem f l) (map mem f r)

let map_path mem f fdd = 
 	let rec aux fdd u = 
 		match peek fdd with 
 		| Leaf v -> value mem (f u v)
 		| Node(b,l,r) ->
 				node mem b (aux l u) (aux r (Update.add b u)) in 
 	aux fdd Update.empty

let rec eval x u = 
 	match peek x with 
 	| Leaf v -> v
 	| Node(fv,l,r) ->
 			eval (if Update.mem fv u then r else l) u


let updates_mem = create_mem Hashtbl.hash Updates.equal

let bot = value updates_mem Updates.empty

let top = value updates_mem (Updates.singleton Update.empty)

let rec neg x =
	match peek x with
	| Leaf v -> 
			value updates_mem 
		  	(if Updates.is_empty v
		   	then Updates.singleton Update.empty
		   	else Updates.empty)
	| Node(b,l,r) -> node updates_mem b (neg l) (neg r)

let apply_union = apply updates_mem Updates.union

let plus x y = apply_union x y 

let seq_one_all a acts = 
	Updates.fold (fun elt acc -> 
		Updates.union acc (Updates.singleton (merge_right_all a elt)) ) acts Updates.empty

let rec seq_act a d =
	match peek d with 
	| Leaf v -> 
		value updates_mem (seq_one_all a v)
	| Node(fv,l,r) -> 
		if Update.mem fv a then seq_act a r else 
		if Update.exists (fun fv' -> compare_field fv' fv = 0) a then 
			seq_act a l
		else node updates_mem fv (seq_act a l) (seq_act a r)

let rec restrict d fv =
	match peek d with 
	| Leaf acts -> node updates_mem fv bot (value updates_mem acts)
	| Node(fv',l,r) ->
		  match compare_field fv fv' with 
		  | 0 -> begin
		 	    match compare_val fv fv' with 
		 	    | Equal -> node updates_mem fv bot r
		 	    | LeftSubsumes -> node updates_mem fv' bot r
		 	    | RightSubsumes -> node updates_mem fv bot r
		 	    | _ ->  restrict l fv
		 	  end
		  | i when i < 0 -> node updates_mem fv bot d
		  | _ -> node updates_mem fv' (restrict l fv) (restrict r fv)

let rec restrict_neg d fv = 
	(* Printf.printf "    restrict_neg [%s] %s\n" (show_fdd show_updates d) (show_field_val "=" fv); *)
	match peek d with 
	| Leaf acts -> node updates_mem fv (value updates_mem acts) bot
	| Node(fv',l,r) ->
			match compare_field fv fv' with 
			 | 0 -> begin
				 	match compare_val fv fv' with
				 	| Equal -> begin
						 	match peek l with 
					 	  | Node(fv'',l'',r'') -> 
					 	    	if compare_field fv'' fv' = 0 && l''== bot then l 
					 	   		else node updates_mem fv' l bot
					 	  | Leaf _ -> node updates_mem fv' l bot 
					 	end
				 	| Less -> if l = bot then d else node updates_mem fv d bot
				 	| Greater -> node updates_mem fv' (restrict_neg l fv) r 
				 	| LeftSubsumes -> restrict_neg l fv (* eventually will introduce test *)
				 	| RightSubsumes -> 
				 			if l = bot then d
				 		  else if r = bot then d 
				 			else node updates_mem fv d bot
			   end
			| i when i < 0 -> node updates_mem fv d bot
			| _ -> node updates_mem fv' (restrict_neg l fv) (restrict_neg r fv)

let seq_apply f = 
	memoize (fun recur x y ->   
    match peek x, peek y with
    | Leaf v, _ ->
      	Updates.fold (fun act fdd -> 
      		plus (f act y) fdd
      	) v bot
    | Node(b,l,r), _ -> 
    	  let left = restrict (recur r y) b in 
    	  let right = restrict_neg (recur l y) b in 
    	  plus left right )

let rec test_apply f act y = 
	match peek y with 
	| Leaf v -> f v
	| Node(b,l,r) -> node updates_mem b (test_apply f act l) (test_apply f act r)

let seq = seq_apply seq_act 

let star x = 
	let rec star_fix d old_d new_d = 
	 	if old_d == new_d then old_d 
		else star_fix d new_d (seq new_d (plus top d))
	in star_fix x bot top

let var fv = node updates_mem fv bot top

let assign fv = value updates_mem (Updates.singleton (Update.singleton fv))

let rec create p = 
	match p with 
	| Zero -> bot
	| One -> top
	| Dup _ -> failwith "[Error]: Dup in local compilation"
	| Test fv -> var fv
	| Assign fv -> assign fv 
	| Seq (a,b) -> seq (create a) (create b) 
	| Sum (a,b) -> plus (create a) (create b)
	| Star p -> star (create p)
	| Neg a -> neg (create a)

let rec replace mem fv x y  =
	match peek x, peek y with
	| Leaf _, _ -> x
	| Node(fv',l,r), Leaf us -> 
			let cmp = compare_field_val fv' fv in 
			if cmp = 0 then (if Updates.equal us Updates.empty then l else r)
			else node mem fv' (replace mem fv l y) (replace mem fv r y)
	| Node(fv1,l1,r1), Node(fv2,l2,r2) ->
			match compare_field fv1 fv2 with
			| 0 -> begin
				  match compare_val fv1 fv2 with 
				  | Equal -> node mem fv1 (replace mem fv l1 l2) (replace mem fv r1 r2) 
				  | Less -> node mem fv1 (replace mem fv l1 y) (replace mem fv r1 l2)
				  | Greater -> node mem fv2 (replace mem fv x l2) (replace mem fv l1 r2) 
				  | _ -> failwith "TODO"
				end
			| i when i < 0 -> node mem fv1 (replace mem fv l1 y) (replace mem fv r1 y) 
			| _ -> node mem fv2 (replace mem fv x l2) (replace mem fv x r2)

let write_rules oc table = 
	let rec aux max_len table = 
		match table with 
		| [] -> ()
		| (tests, acts)::tl -> 
        let test_str = show_list (show_field_val "=") tests in
        let act_str = show_updates acts in
        let num_spaces = (max_len + 2) - (String.length test_str) in
        let spaces = String.make num_spaces ' ' in
 		    Printf.fprintf oc "%s%s==>  %s\n" test_str spaces act_str;
			aux max_len tl in 
  let max_len = 
    List.fold_left (fun acc (ts,_) -> 
      let str = show_list (show_field_val "=") ts in
      let len = String.length str in 
      max len acc) 0 table in
  let border = String.make (max_len*2 + 7) '=' in
  Printf.fprintf oc "%s\n" border;
	aux max_len table;
  Printf.fprintf oc "%s\n" border

let rec is_subset_of fvs1 fvs2 = 
	match fvs1, fvs2 with 
	| [], [] -> true
	| [], hd::tl -> false
	| hd::tl, [] -> true
	| fv1::tl1, fv2::tl2 -> 
			let cmp = compare_field_val fv1 fv2 in 
			if cmp = 0 then is_subset_of tl1 tl2 
			else if cmp < 0 then false 
			else false

let rec get_all_values whichfv rules strs =
	let aux fvs = 
		List.fold_left (fun acc fv -> 
			if compare_field fv whichfv = 0 then 
				StrSet.add (Syntax.get_value fv) acc
			else acc) StrSet.empty fvs in 
	match rules with 
	| [] -> strs
	| (fvs,us)::tl -> 
			let states_us = 
				Updates.fold (fun u acc -> 
					Update.fold (fun fv acc' ->
						if compare_field fv whichfv = 0 then 
							StrSet.add (Syntax.get_value fv) acc'
						else acc') u acc) us StrSet.empty in
			get_all_values whichfv tl (StrSet.union strs (StrSet.union (aux fvs) states_us))

let get_all_values whichfv rules =
	get_all_values whichfv rules StrSet.empty

let is_value whichfv fv = 
	compare_field whichfv fv = 0

let differ_only_by_value whichfv fvs1 fvs2 = 
	let ds1, others1 = List.partition (is_value whichfv) fvs1 in
	let ds2, others2 = List.partition (is_value whichfv) fvs2 in
	match ds1, ds2 with 
	| [v1], [v2] -> 
			let s1 = Syntax.get_value v1 in 
			let s2 = Syntax.get_value v2 in 
			if s1 <> s2 && others1 = others2 then Some(s1,s2) else None
	| _ -> None

let same_value whichfv (fvs, us) = 
	match fvs with 
	| v::_ when compare_field v whichfv = 0 ->
			let s = Syntax.get_value v in
			let is_good = 
				Updates.for_all (fun u -> 
					Update.for_all (fun fv ->
						if compare_field fv whichfv = 0 then 
							let s' = get_value fv in 
							s = s'
						else true) u) us in
			if is_good then 
				Some (map_updates (Update.filter (fun fv -> not (is_value whichfv fv) ) ) us)
			else None
	| _ -> 
			if Updates.for_all (fun u -> not (Update.exists (is_value whichfv) u)) us
			then Some us 
			else None

let find_most_equal ls = 
	assert (List.length ls > 0);
	let repeats (_,x) = 
		List.fold_left (fun acc (_,y) -> 
			if Updates.equal x y then acc + 1 else acc) 0 ls in 
	let hd = List.hd ls in
	List.fold_left (fun (v,count) x -> 
		let n = repeats x in
		if n > count then (x,n) else (v,count)) (hd, repeats hd) ls 

let compress_state_when_possible whichfv pending =
	assert (List.length pending > 0);
	let same, diff = List.partition (fun x -> Option.is_some (same_value whichfv x)) pending in
	let (best_eq, nbest_eq) = find_most_equal pending in
	let (_, be_us) = best_eq in
	let eq, uneq = List.partition (fun (_,us) -> Updates.equal be_us us) pending in 
	if List.length same = 0 then 
		(if nbest_eq > 1 then 
			let (fvs,us) = List.hd eq in 
			uneq @ [(List.tl fvs, us)]
		else pending)
	else
		let (best_s, nbest_s) = find_most_equal same in 
		let (_, bs_us) = best_s in
		let same, other = List.partition (fun (_,us) -> Updates.equal bs_us us) same in
		if nbest_s > nbest_eq then 
			let (fvs,us) = List.hd same in
			let us = map_updates (fun u -> Update.filter (fun fv -> not (is_value whichfv fv)) u) us in 
			other @ diff @ [(List.tl fvs, us)]
		else if nbest_eq > 1 then 
			let (fvs,us) = List.hd eq in 
			uneq @ [(List.tl fvs, us)]
		else pending

let reduce_tagging whichfv rules = 
	let all_states = get_all_values whichfv rules in 
	let rec aux rules pending states =
		let (p_fvs, p_us) = List.hd pending in
		match rules with 
		| [] -> pending
		| ((fvs, us) as hd)::tl ->
			match (differ_only_by_value whichfv p_fvs fvs) with 
			| None ->
					let pending = List.rev pending in		
					if StrSet.equal states all_states then begin
						(compress_state_when_possible whichfv pending) @ (aux tl [hd] StrSet.empty)
					end
					else pending @ (aux tl [hd] StrSet.empty)
			| Some (s1, s2) ->
					let seen = StrSet.add s1 (StrSet.add s2 states) in 
					aux tl (hd::pending) seen in
	match rules with 
	| [] -> []
	| hd::tl -> aux tl [hd] StrSet.empty

let rec remove_redundant_drops x acc =
	match x with
  | [] -> unreachable ()
 	| (fvs1,us1)::tl -> begin
 			match tl with 
 			| [] -> List.rev ((fvs1,us1) :: acc)
 			| (fvs2,us2)::tl' ->
 				let is_drop = Updates.is_empty us1 in
 				let is_subset = is_subset_of (List.rev fvs1) (List.rev fvs2) in 
 				if is_drop && (not is_subset) then 
 					remove_redundant_drops tl acc
 				else remove_redundant_drops tl ((fvs1,us1)::acc)
 		end

let reduce_drop_rules rules = 
 	match List.rev rules with 
 	| [] -> unreachable ()
 	| (fvs,us)::_ -> 
 		  if Updates.is_empty us then 
 			  remove_redundant_drops rules []
 		  else List.rev rules

let rules x =  
	let rec aux fdd tests = 
		match peek fdd with 
		| Leaf v -> (bot, (tests,v), true)
		| Node(b,l,r) -> 
			(match peek r with 
			 | Leaf v -> (l, (b::tests,v), false)
			 | _ ->   
				let r', rule, _ = aux r (b::tests) in 
				(node updates_mem b l r', rule, false) ) in 
	let fdd = ref x in 
	let rules = ref [] in 
	let loop = ref true in
	while !loop do 
		let fdd', rule, last = aux !fdd [] in 
		fdd := fdd';
		rules := rule :: !rules;
		loop := not last
	done;
	let dummy_state = State (-1) in
	let rules = List.rev !rules in 
	let rules_o1 = reduce_drop_rules rules in
  let rules_o2 = ref (reduce_tagging dummy_state rules_o1) in
  let o, n = ref (List.length rules_o1), ref (List.length !rules_o2) in 
  while !o <> !n do 
    o := !n; 
    rules_o2 := reduce_tagging dummy_state !rules_o2;
    n := List.length !rules_o2
  done;
	let num_tags = StrSet.cardinal (get_all_values dummy_state !rules_o2) in
	(rules, rules_o1, !rules_o2, num_tags)


module Test = struct
	open Input 

	let assert_equal x y = 
		let trmx, _ = x |> read_from_str |> extract_queries in 
		let trmy, _ = y |> read_from_str |> extract_queries in 
		let fddx = create trmx in 
		let fddy = create trmy in 
		if fddx != fddy then 
		  Printf.printf 
		    "  Failed test: [%s] = [%s]\n"
			  (show_fdd show_updates fddx) 
			  (show_fdd show_updates fddy)
		else ()

	let tests = [
		(* additive identity/zero *)
		("id + sw=A", "id");
		("sw=A + id", "id");
		("drop + drop", "drop");
		("drop + id", "id");
		("id + drop", "id");
		("drop + sw=A", "sw=A");
		("sw=A;src=10 + id", "id");
		(* commutativity *)
		("pt=1 + src=10", "src=10 + pt=1");
		(* associativity *)
		("sw=A + (pt=1 + src=10)", "(sw=A + pt=1) + src=10");
		("(sw=A;pt=1);src=10", "sw=A;(pt=1;src=10)");	
		(* distributivity *)
		("src=10;(sw=A + sw=B)", "src=10;sw=A + src=10;sw=B");
		("(sw=A + sw=B);src=10", "sw=A;src=10 + sw=B;src=10");
		(* contradiction, excluded-middle, idempotency *)
		("sw=A;sw=B", "drop");
		("sw=A;src=10;sw=B", "drop");
		("sw=A;-sw=A", "drop");
		("sw=A + -sw=A", "id");
		("sw=A;sw=A", "sw=A");
		(* Demorgan's laws *)
		("-(src=10 and pt=1)", "(-src=10) + (-pt=1)");
		("-(sw=A and src=10);sw=A", "(-src=10);sw=A");
		(* disjoint fields *)
		("-sw=A;sw=A", "drop");
		("-sw=A;sw=B", "sw=B");
		("sw=B;-sw=A", "sw=B");
		(* assignment ordering *)
		("sw<-A;sw<-B", "sw<-B");
		("sw<-A;sw=A", "sw<-A");
		("sw<-A;sw=B", "drop");
		(* Kleene star *)
		("(sw=A)*", "id");
		("(sw=B + src=10)*", "id");
		("(sw=A;sw<-B)*", "id + sw=A;sw<-B");
		(* Prefixes conjunction *)
		("dst=10.0.0.1; dst=10.0.0.2", "drop");
		("dst=1.2.0.0/16; dst=1.2.0.0/24", "dst=1.2.0.0/24");
		("dst=1.2.3.0/24; dst=1.2.0.0/16", "dst=1.2.3.0/24");
		("-dst=1.2.3.0/24; dst=1.2.0.0/16", "dst=1.2.0.0/16");
		("dst=1.2.3.0/24; -dst=1.2.0.0/16", "drop");
		("-dst=1.2.3.0/24; -dst=1.2.0.0/16", "-dst=1.2.0.0/16");
		("-dst=1.2.0.0/16; -dst=1.2.3.0/24", "-dst=1.2.0.0/16");
		("-dst=10.0.0.0; -dst=1.0.0.0; -dst=10.0.0.0/16", "-dst=10.0.0.1/16; -dst=1.0.0.0");
		("-dst=10.0.0.0; -dst=1.0.0.0/24; -dst=10.0.0.0/16", "-dst=10.0.0.1/16; -dst=1.0.0.0/24");
		("dst=10.0.0.1 + dst=10.0.0.2", "dst=10.0.0.2 + dst=10.0.0.1");
		(* Prefix disjunction *)
		("dst=1.0.0.0/16 + dst=1.0.0.0", "dst=1.0.0.0/16");
		("dst=1.0.0.0 + dst=1.0.0.0/16", "dst=1.0.0.0/16");
		("dst=2.0.0.0/16 + dst=1.0.0.0 + dst=2.0.0.0", "dst=2.0.0.0/16 + dst=1.0.0.0");
		("-dst=1.0.0.1 + -dst=1.0.0.0/16", "-dst=1.0.0.1");
		("-dst=1.0.0.0/16 + -dst=1.0.0.1", "-dst=1.0.0.1");
	]

	let unit_tests () =
		(* Identities *) 
		let rec aux tests = 
			match tests with 
			| [] -> () 
			| (x,y)::ts -> begin
					assert_equal x y; 
					aux ts 
				end 
		in 
		print_endline "Testing Fdd...";
		aux tests;
		(* Random tests *)
		for i=0 to 1000000 do 
			let trm = Syntax.Arbitrary.gen_term_local () in 
			ignore (create trm)
		done
end
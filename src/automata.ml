open Syntax
open Common
open Fdd

(* Types, Modules, Signatures, and
   helper functions for automata. *)

module Types = struct

  type 't automaton = {
    initial: int;
    accept: (updates fdd) Ptmap.t;
    trans: ('t fdd) Ptmap.t;
  }

  module PDerivType =
    struct
      type t = term * int * term
      let compare (d1,l1,k1) (d2,l2,k2) =
        let cmp = l1 - l2 in
        if cmp = 0 then
          (let cmp' = compare_term d1 d2 in
          if cmp' = 0 then compare_term k1 k2 else cmp')
        else cmp
    end

  module PDeriv = Set.Make(PDerivType)

  module type HashCons = sig
    type t
    val compare: t -> t -> int
    val hash: t -> int
  end

  module TransitionState(H: HashCons) = struct
    module M = struct
      type t = H.t * update
      let compare (x,u1) (y,u2) =
        let cmp = H.compare x y in
        if cmp = 0 then
          Update.compare u1 u2
        else cmp
    end
    module S = Set.Make(M)
    let hash x =
      let aux (y,us)
        acc = (H.hash y) + (Syntax.hash_update us) + acc
      in S.fold aux x 0
    include S
  end

  module TInt = TransitionState(NatType)
  module TSet = TransitionState(NatSet)
  module TPair = TransitionState(NatType2)

  type t = TInt.t automaton

  type orientation =
    | Left of TInt.t
    | Right of TInt.t

  let show_int_trans =
    let aux (i,u) =
      "(" ^ (string_of_int i) ^ "," ^ (show_update u) ^ ")"
    in show_set aux TInt.fold

  let equal_orientation o1 o2 =
    match o1, o2 with
    | Left x, Left y
    | Right x, Right y -> TInt.equal x y
    | _, _ -> false

  let hash_orientation o =
    match o with
    | Left x -> 7 + TInt.hash x
    | Right x -> 17 + TInt.hash x

  module Mem = struct
    let int_mem = Fdd.create_mem TInt.hash TInt.equal
    let set_mem = Fdd.create_mem TSet.hash TSet.equal
    let pair_mem = Fdd.create_mem TPair.hash TPair.equal
    let lr_mem = Fdd.create_mem hash_orientation equal_orientation
  end
  include Mem

end
include Types


(* Specialize apply to memoize applications *)
let apply_tint_union = Fdd.apply int_mem TInt.union

(* Simpler wrapper around automaton state lookup *)
let lookup map i =
  try Ptmap.find i map
  with Not_found ->
    failwith ("[lookup]: state does not exist: " ^ (string_of_int i))


(* Build an automaton lazily from an initial state,
   an acceptance function, and a transition function.
   States are reindexed as integers as the automaton
   is constructed. *)

module Builder(O: Set.OrderedType) = struct
  module S = Set.Make(O)
  module R = Reindex.Make(O)
  type state = O.t

  let build (explore: 't -> S.t)
            (adjust: R.t -> 't -> 'r)
            (mem: 'r mem)
            (initial: state)
            (accept: state -> updates fdd)
            (trans: state -> 't fdd) : 'r automaton =
    let indexer = R.create () in
    let i = R.get_idx indexer initial in
    let a_map = ref Ptmap.empty in
    let t_map = ref Ptmap.empty in
    let seen = ref (NatSet.singleton i) in
    let todo = Queue.create () in
    let add_state s =
      let i = R.get_idx indexer s in
      if not (NatSet.mem i !seen)
      then (seen := NatSet.add i !seen; Queue.push i todo) in
    Queue.push i todo;
    while not (Queue.is_empty todo) do
      let i = Queue.pop todo in
      let elt = R.get_val indexer i in
      let trans_elt = trans elt in
      let ts = Fdd.elements trans_elt in
      let unq = Bag.fold (fun acc e -> S.union (explore e) acc) S.empty ts in
      S.iter add_state unq;
      a_map := Ptmap.add i (accept elt) !a_map;
      t_map := Ptmap.add i (Fdd.map mem (adjust indexer) trans_elt) !t_map;
    done;
    {initial = 1; accept = !a_map; trans = !t_map}
end

(* Wraps the Builder Module to provide the [explore]
   and [adjust] functions for automata construction.
   These help explore new states from arbitrary transition
   type (fdd leaf), and reindex the automaton respectively. *)

module Build(O: Set.OrderedType)(T: Set.S with type elt = O.t * update) = struct
  module B = Builder(O)
  module S = Set.Make(O)

  let explore nt =
    let update (j,_) acc = S.add j acc in
    T.fold update nt S.empty

  let build =
    let change indexer (i,u) acc = TInt.add (B.R.get_idx indexer i, u) acc in
    let reindex indexer nt = T.fold (change indexer) nt TInt.empty in
    B.build explore (fun indexer x -> reindex indexer x)
end

(* Specializes the Builder Module for building automata lazily over state types:
   int: normal NetKAT automata
   int pair (x,y): For constructing automata intersection
   int set {x,...,y}: For determinizing automata
   Also allows tracing an automaton to run a user-defined function f
   over each reachable state. *)

module Lazy = struct
  module BInt = Build(NatType)(TInt)
  module BSet = Build(NatSet)(TSet)
  module BPair = Build(NatType2)(TPair)

  let trace_init (explore: 't -> NatSet.t)
                 (run: 't automaton -> int -> unit)
                 (fsa: 't automaton)
                 (init: int) : unit =
    let seen = ref (NatSet.singleton init) in
    let todo = Queue.create () in
    let add_state i =
      if not (NatSet.mem i !seen)
      then (seen := NatSet.add i !seen; Queue.push i todo) in
    Queue.push init todo;
    while not (Queue.is_empty todo) do
        let i = Queue.pop todo in
        let ts = Fdd.elements (lookup fsa.trans i) in
        run fsa i;
        let unq = Bag.fold (fun acc e -> NatSet.union (explore e) acc) NatSet.empty ts in
        NatSet.iter add_state unq;
    done

  let build_int = BInt.build int_mem
  let build_set = BSet.build int_mem
  let build_pair = BPair.build int_mem

  let trace run dfa = trace_init BInt.explore run dfa
end
include Lazy


let print auto =
  let aux auto i =
    Printf.printf "State: %d\n------------------\n" i;
    Printf.printf "Accept: %s\n" (show_fdd show_updates (lookup auto.accept i));
    Printf.printf "Transitions: %s\n" (show_fdd show_int_trans (lookup auto.trans i));
    Printf.printf "------------------\n\n%!";
  in trace aux auto auto.initial


(* Determinize an automaton by providing a new
   initial state, acceptance function, and transition
   function as sets of old states *)

module Determinize = struct
  let collect_by_update (us: TInt.t) : TSet.t =
    let res = ref TSet.empty in
    let aux (i,u) =
      let other = TInt.filter (fun (j,v) -> i <> j && Update.equal u v) us in
      let states = TInt.fold (fun (j,v) acc -> NatSet.add j acc) other NatSet.empty in
      res := TSet.add (NatSet.add i states, u) !res in
    TInt.iter aux us; !res

  let determinize (auto: t) : t =
    let accept ls =
      let aux l acc = Fdd.plus (lookup auto.accept l) acc in
      NatSet.fold aux ls Fdd.bot in
    let trans ls  =
      let base = Fdd.value int_mem TInt.empty in
      let all = NatSet.fold (fun l acc ->
        let t = lookup auto.trans l in
        apply_tint_union t acc) ls base in
      Fdd.map set_mem collect_by_update all in
    build_set (NatSet.singleton (auto.initial)) accept trans
end
include Determinize


(* Heuristic minimization of a determinized automaton.
   Merges equivalent states, detects extraneous garbage
   states that never accept, etc. *)

module Minimize = struct
  module F1 = struct type t = updates end
  module F2 = struct type t = TInt.t end
  module FddMap2 = Map.Make(FddType2(F1)(F2))

  let concretize_to_map (auto: t) : int NatMap.t =
    let finder = ref FddMap2.empty in
    let mapper = ref NatMap.empty in
    let aux auto i =
      let pair = (lookup auto.accept i, lookup auto.trans i)  in
      try
        let x = FddMap2.find pair !finder in
        mapper := NatMap.add i x !mapper
      with Not_found ->
        finder := FddMap2.add pair i !finder;
        mapper := NatMap.add i i !mapper in
    trace aux auto auto.initial; !mapper

  let rewrite_states (auto: t) (map: int NatMap.t) : t =
    let change_transition us =
      let aux (i,u) acc =
        let i' = try NatMap.find i map with _ -> i in
        TInt.add (i', u) acc in
      TInt.fold aux us TInt.empty in
    let trans i = Fdd.map int_mem change_transition (lookup auto.trans i) in
    build_int auto.initial (lookup auto.accept) trans

  let merge_equal_states (auto: t) : t =
    rewrite_states auto (concretize_to_map auto)

  let find_drop_states (auto: t) : NatSet.t =
    let aux i fdd acc =
      if fdd == Fdd.bot then
        NatSet.add i acc
      else acc in
    Ptmap.fold aux auto.accept NatSet.empty

  let is_dead (drops: NatSet.t) (auto: t) (i: int) : bool =
    let all_dead = ref true in
    let aux auto i =
      all_dead := !all_dead && NatSet.mem i drops in
    trace aux auto i;
    !all_dead

  let find_dead_states (auto: t) : NatSet.t =
    let drops = find_drop_states auto in
    NatSet.filter (is_dead drops auto) drops

  let delete trans i =
    let aux us =
      TInt.fold (fun (j,u) acc ->
        if i = j then acc else TInt.add (j,u) acc) us TInt.empty in
    Fdd.map int_mem aux trans

  let num_states auto =
    Ptmap.fold (fun _ _ acc -> acc + 1) auto.accept 0

  let remove_dead_state (i: int) (auto: t) : t =
    if i = auto.initial then auto
    else
      let a = Ptmap.remove i auto.accept in
      let t = Ptmap.remove i auto.trans in
      let t =
        Ptmap.fold (fun j fdd acc ->
          Ptmap.add j (delete fdd i) acc) t Ptmap.empty in
      {initial=auto.initial; accept=a; trans=t}

  let remove_dead_states (auto: t) : t =
    let dead = find_dead_states auto in
    NatSet.fold remove_dead_state dead auto

  let merge_gb_states (auto: t) : t =
    let drops = find_drop_states auto in
    let gb =
      NatSet.filter (fun i ->
        let t = Ptmap.find i auto.trans in
        let elts = Fdd.elements t in
        if Bag.length elts = 1 then
          let x = Bag.first elts in
          if TInt.cardinal x = 1 then
            let (j,_) = TInt.choose x in
            i = j
          else false
        else false) drops in
    if NatSet.cardinal gb > 1 then
      let x = NatSet.choose gb in
      let gb = NatSet.remove x gb in
      let map = NatSet.fold (fun g acc -> NatMap.add g x acc) gb NatMap.empty in
      rewrite_states auto map
    else
      auto

  let minimize (auto: t) : t =
    auto |> merge_gb_states |> merge_equal_states

  let minimize2 (auto: t) : t =
    auto |> remove_dead_states
end
include Minimize


(* Creates a new automaton that is the intersection of
   the two old automata. Placeholders are substituted for
   the concrete value provided by the query automata. *)

module Intersection = struct
  let cross_product x y =
    let aux l r =
      try
        let (i,_) = TInt.choose l in
        let aux (j,u) acc = TPair.add ((i,j),u) acc in
        TInt.fold aux r TPair.empty
      with _ -> TPair.empty in
    match x,y with
    | Left l, Right r -> aux l r
    | Right r, Left l -> aux l r
    | _ -> unreachable ()

  let apply_inter = Fdd.apply pair_mem cross_product

  let tag_and_apply l r =
    let annot_l = Fdd.map lr_mem (fun us -> Left us) l in
    let annot_r = Fdd.map lr_mem (fun us -> Right us) r in
    apply_inter annot_l annot_r

  let replace mem p qry_fdd fdd =
    Fdd.replace mem (Placeholder p) fdd qry_fdd

  let merge_placeholder (p:int) (qry: t) (auto: t) : t =
    let initial = (qry.initial, auto.initial) in
    let accept (x,y) =
      replace updates_mem p (lookup qry.accept x) (lookup auto.accept y) in
    let trans (x,y) =
      let q = lookup qry.trans x in
      let t = replace int_mem p (lookup qry.accept x) (lookup auto.trans y) in
      tag_and_apply q t in
    build_pair initial accept trans

  let intersect (a1: t) (a2: t) : t =
    let initial = (a1.initial, a2.initial) in
    let accept (x,y) =
      Fdd.seq (lookup a1.accept x) (lookup a2.accept y) in
    let trans (x,y) =
      let l = lookup a1.trans x in
      let r = lookup a2.trans y in
      tag_and_apply l r in
    build_pair initial accept trans
end
include Intersection


(* Create NetKAT, LTL, and Temporal NetKAT
   automata from syntax terms. The instrument function
   extracts the LTL parts and replaces them with abstract placeholders.
   These are substituted away during the intersection. *)

module Create = struct
  module NetKAT = struct
    let dotr trips p =
      let f (q,l,k) r = (q,l, Syntax.seq k r) in
      PDeriv.fold (fun trip acc -> PDeriv.add (f trip p) acc) trips PDeriv.empty

    let dotl p trips =
      let f p (q,l,k) = (Syntax.seq p q,l,k) in
      PDeriv.fold (fun trip acc -> PDeriv.add (f p trip) acc) trips PDeriv.empty

    let rec obs p =
      match p with
      | Test _ | Assign _ | Neg _ | Zero | One -> p
      | Dup _ -> zero
      | Sum (q,r) -> Syntax.sum (obs q) (obs r)
      | Seq (q,r) -> Syntax.seq (obs q) (obs r)
      | Star q -> Syntax.star (obs q)

    let rec deriv p =
      match p with
      | Test _ | Assign _ | Neg _ | Zero | One -> PDeriv.empty
      | Dup l -> PDeriv.singleton (one, l, one)
      | Sum (q,r) -> PDeriv.union (deriv q) (deriv r)
      | Seq (q,r) -> PDeriv.union (dotr (deriv q) r) (dotl (obs q) (deriv r))
      | Star q -> dotr (dotl (obs p) (deriv q)) p

    let create (trm: term) : t =
      let add (d,l,k) cmap = NatMap.add l k cmap in
      let partials = add (zero,0,trm) (PDeriv.fold add (deriv trm) NatMap.empty) in
      let accept l = NatMap.find l partials |> obs |> Fdd.create in
      let trans l =
        let add_state l us =
          Updates.fold (fun u acc -> TInt.add (l,u) acc) us TInt.empty
        in
        let cont = NatMap.find l partials in
        let dkl = cont |> deriv in
        let empty = Fdd.value int_mem TInt.empty in
        let partial (d,l,k) acc =
          let fdd = Fdd.create d in
          let dl = Fdd.map int_mem (add_state l) fdd in
          apply_tint_union dl acc in
        PDeriv.fold partial dkl empty in
      build_int 0 accept trans
  end
  include NetKAT

  module TemporalLogic = struct
    let add_accepting_state ~only_accept ~final_loop (auto: t)  =
      let accept j =
        if j = 0 then
          Fdd.value updates_mem (Updates.singleton Update.empty)
        else if only_accept then
          Fdd.value updates_mem Updates.empty
        else lookup auto.accept j in
      let trans j =
        if j = 0 then
          Fdd.value int_mem (if final_loop then TInt.singleton (0,Update.empty) else TInt.empty)
        else
          let a = lookup auto.accept j in
          let go_to_final u acc = TInt.add (0,u) acc in
          let aux us = Updates.fold go_to_final us TInt.empty in
          let new_t = Fdd.map int_mem aux a in
          let new_t = apply_tint_union (lookup auto.trans j) new_t in
          Fdd.map int_mem (fun us -> 
            if TInt.exists (fun (i,_) -> i = 0) us then
              TInt.filter (fun (i,_) -> i = 0) us
            else us) new_t in
      build_int auto.initial accept trans

    let negate (auto: t) : t =
      let accept = Ptmap.map Fdd.neg auto.accept in
      {initial = auto.initial; accept; trans = auto.trans}

    let id () =
      let accept _ = Fdd.top in
      let trans _ = Fdd.value int_mem (TInt.singleton (0, Update.empty)) in
      build_int 0 accept trans

    let drop () =
      let accept _ = Fdd.bot in
      let trans _ = Fdd.value int_mem (TInt.singleton (0, Update.empty)) in
      build_int 0 accept trans

    let test fv =
      let accept i = Fdd.create (test fv) in
      let trans i = Fdd.value int_mem (TInt.singleton (0,Update.empty)) in
      build_int 0 accept trans

    let rec create_temporal (tt: tterm) : t =
      let res =
        match tt with
        | Tzero -> drop ()
        | Tone -> id ()
        | Ttest fv -> test fv
        | Tor (a,b) -> create_temporal (Tneg (Tand (Tneg a, Tneg b)))
        | Tand (a,b) -> intersect (create_temporal a) (create_temporal b)
        | Tneg a -> negate (create_temporal a)
        | Tlast a ->
            create_temporal a
            |> add_accepting_state ~only_accept:true ~final_loop:false
            |> determinize
        | Tever a ->
            create_temporal a
            |> add_accepting_state ~only_accept:false ~final_loop:true
            |> determinize
        | Talways a -> create_temporal (Tneg (Tever (Tneg a)))
        | Twlast a -> create_temporal (Tneg (Tlast (Tneg a)))
        | Tstart ->
            let accept i = if i = 0 then Fdd.top else Fdd.bot in
            let trans i =
              if i = 0 then Fdd.value int_mem (TInt.singleton (1,Update.empty))
              else Fdd.value int_mem (TInt.singleton (1,Update.empty)) in
            build_int 0 accept trans |> determinize
        | Tassign _ | Tseq _ | Tplus _ | Tstar _ | Tdup -> unreachable () in
        minimize res
  end
  include TemporalLogic


  module TemporalNetKAT = struct
    let reindex (auto: t) : t =
      build_int auto.initial (lookup auto.accept) (lookup auto.trans)

    let is_drop (auto: t) : bool =
      Ptmap.fold (fun i a acc -> acc && a == Fdd.bot) auto.accept true

    let check_disjoint_conserv qry1 qry2 =
      intersect qry1 qry2 |> minimize |> is_drop

    let instrument (opt: bool) tterm =
      let trm, tts = Syntax.extract_queries tterm in
      let qry_nfas =
        Profile.profile "[Temporal Automaton]"
          (NatMap.map (fun tt -> create_temporal tt))
          tts in
      let replace = ref
        (NatMap.fold (fun i qry acc ->
          NatMap.add i (Test (Placeholder i)) acc) qry_nfas NatMap.empty) in
      if opt then
        NatMap.iter (fun i qry1 ->
          NatMap.iter (fun j qry2 ->
            if i <> j then
              if check_disjoint_conserv qry1 qry2 then
                let x = NatMap.find i !replace in
                let y = Seq(x, (Neg (Test (Placeholder j)))) in
                replace := NatMap.add i y !replace
              else ()
            else ()) qry_nfas) qry_nfas;
      let trm = Syntax.replace_placeholders trm !replace in
      let nfa = Profile.profile "[Policy Automaton]" create trm in
      let dfa = Profile.profile "[Determinize]" determinize nfa in
      let dfa = if opt then Profile.profile "[Minimize]" minimize dfa else dfa in
      let dfa = Profile.profile "[Intersection]"
        (NatMap.fold (fun i qry acc ->
          acc |> merge_placeholder i qry) qry_nfas) dfa in
      let dfa = if opt then Profile.profile "[Minimize]" minimize2 dfa else dfa in
      let dfa = reindex dfa in
      let nqueries = NatMap.fold (fun _ _ acc -> acc + 1) tts 0 in
      dfa, nqueries
  end
  include TemporalNetKAT
end
include Create


(* Follows the Fast Compiler for NetKAT paper
   to compile a NetKAT automaton to concrete
   forwarding rules for an Openflow-like language *)

module Compile = struct
  let iter_nfa_trans f trans =
    let aux_inner x = f x in
    let aux us = TInt.iter aux_inner us in
    let elts = Fdd.elements trans in
    Bag.iter aux elts

  let is_link nfa i =
    let t = lookup nfa.trans i in
    let res = ref false in
    let is_sw fv = match fv with Sw _ -> true | _ -> false in
    iter_nfa_trans (fun (_,u) -> res := !res || Update.exists is_sw u) t;
    !res

  let adjust_next_hop nfa i =
    let aux path us =
      TInt.fold (fun (j,u) acc ->
        if not (is_link nfa j) then TInt.add (j,u) acc
        else
          let t = lookup nfa.trans j in
          let valuation = Syntax.merge_right_all path u in
          let x = Fdd.eval t valuation in
          if TInt.is_empty x then acc
          else
            let (k,_) = TInt.choose x in
            TInt.add (k,u) acc
        ) us TInt.empty in
    Fdd.map_path int_mem aux (lookup nfa.trans i)

  let add_state_tag i trans =
    let aux_inner (i',u) =
      if i' <> i then
        let s = Syntax.State i' in
        Update.add s u 
      else u in
    let aux us =
      let f u acc = Updates.add (aux_inner u) acc in
      TInt.fold f us Updates.empty in
    Fdd.map Fdd.updates_mem aux trans

  let test_state i fdd =
    let s = Syntax.State i in
    Fdd.seq (Fdd.var s) fdd

  let is_state = function
    | State _ -> true
    | _ -> false

  let get_unused tagged untagged u acc =
    let u' = Update.filter (fun fv -> not (is_state fv)) u in
    let eq_untagged = Updates.filter (Update.equal u') untagged in
    Updates.union acc eq_untagged

  let remove_subsets us =
    let tagged, untagged = Updates.partition (Update.exists is_state) us in
    let redundant = Updates.fold (get_unused tagged untagged) tagged Updates.empty in
    Updates.diff us redundant

  let compile auto =
    let local = ref Fdd.bot in
    let aux auto i =
      if is_link auto i then ()
      else
        let a = lookup auto.accept i in
        let t = adjust_next_hop auto i |> add_state_tag i in
        let both = test_state i (Fdd.plus a t) in
        let pruned = Fdd.map Fdd.updates_mem remove_subsets both in
        local := Fdd.plus !local pruned in
    trace aux auto auto.initial; !local
end
include Compile


(* Unit tests are given as equivalences from the paper.
   In each of these cases, the compiler produces equivalent
   automata for the equivalent policies.  *)

module Test = struct
  open Input

  let assert_equal x y =
    let dfax, _ = instrument true x in
    let dfay, _ = instrument true y in
    let cx = compile dfax in
    let cy = compile dfay in
    if cx <> cy then
      Printf.printf
        "\n  Failed test:\n%s -- %s\n%s -- %s\n\n"
        (show_tterm x)
        (show_tterm y)
        (show_fdd show_updates cx)
        (show_fdd show_updates cy)

  let tests = [
    (* Temporal NetKAT Axioms *)
    ("last(sw=A and pt=1)", "last(sw=A) and last(pt=1)");
    ("last(sw=A or pt=1)", "last(sw=A) or last(pt=1)");
    ("wlast(id)", "id");
    ("-ever(-sw=A)", "always(sw=A)");
    ("always(sw=A)", "sw=A and wlast(always(sw=A))");
    ("ever(sw=A)", "sw=A or last(always(sw=A))");
    ("wlast(sw=A)", "start or last(sw=A)");
    ("last(sw=A)", "-start and last(sw=A)");
    ("-last(sw=A)", "start or last(-sw=A)");
    ("start;last(sw=A)", "drop");
    ("start;wlast(sw=A)", "start");
    ("start;ever(sw=A)", "start;sw=A");
    ("start;always(sw=A)", "start;sw=A");
    ("always(ever(sw=A))", "ever(start and sw=A)");
    ("ever(always(sw=A))", "ever(start and sw=A)");
    ("ever(start)", "id");
    ("sw<-A;dup;start", "drop");
    (* Other tests - Ever *)
    ("sw=A;dup", "sw=A;dup;last(sw=A)");
    ("sw=A;dup;sw=B;dup", "sw=A;dup;sw=B;dup;ever(sw=A)");
    ("sw=A;dup;sw=B;dup;sw=C;dup", "sw=A;dup;sw=B;dup;sw=C;dup;ever(sw=A)");
    (* Other tests - Always *)
    ("sw=A", "always(sw=A)");
    ("sw=A;dup;sw=A;dup", "sw=A;dup;sw=A;always(sw=A);dup");
    (* Other tests - Last *)
    ("dup;last(sw=A)", "sw=A;dup");
    ("sw<-A;dup;last(sw=A)", "sw<-A;dup")
  ]

  let unit_tests () =
    (* Identities *)
    let rec aux tests =
      match tests with
      | [] -> ()
      | (x,y)::ts ->
          let x = read_from_str x in
          let y = read_from_str y in
          assert_equal x y;
          aux ts in
    print_endline "Testing Automata...";
    aux tests;
    (* Random tests *)
    for i=0 to 100000 do
      let trm = Syntax.Arbitrary.gen_tterm () in
      ignore (instrument true trm)
    done
end

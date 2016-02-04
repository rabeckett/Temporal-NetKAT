open Common
open Syntax
open Input


let test = ref false
let stats = ref false
let in_file = ref None
let out_file = ref None
let usage = "Usage: tkat [options]"

let params = [
    ("--in", Arg.String (fun s -> in_file := Some s), "  Input file name (default stdin)");
    ("--out", Arg.String (fun s -> out_file := Some s), "  Output file name (default none)");
    ("--test", Arg.Unit (fun _ -> test := true), "  Runs unit tests" );
    ("--stats", Arg.Unit (fun n -> stats := true), "  Output performance statistics as csv to stdout");
  ]

let get_time tbl str = 
  try string_of_float (Hashtbl.find tbl str)
  with _ -> "null"

let _ =
  try begin
    Arg.parse params (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage;
    if !test then begin
      Fdd.Test.unit_tests ();
      Automata.Test.unit_tests ();
      exit 0
    end
    else
      let start = Sys.time () in
      let tt = 
        match !in_file with 
        | None -> Input.read_from_in stdin
        | Some f -> Input.read_from_file f in
      let trm, map = Syntax.extract_queries tt in
      let is_local = (NatMap.is_empty map) && (Syntax.is_local trm) in
      
      let rules = ref [] in
      let num_rules = ref 0 in
      let num_rules_opt1 = ref 0 in 
      let num_rules_opt2 = ref 0 in
      let sz_trm = ref 0 in
      let sz_qry = ref 0 in
      let num_tags = ref 0 in
      let nqueries = ref 0 in

      (if is_local then 
        begin
          let fdd = Profile.profile "[Extraction]" Fdd.create trm in 
          let (rs, rs_opt1, rs_opt2, ntags) = Profile.profile "[Rule Generation]" Fdd.rules fdd in
          rules := rs_opt2;
          num_rules := List.length rs;
          num_rules_opt1 := List.length rs_opt1;
          num_rules_opt2 := List.length rs_opt2;
          sz_trm := Syntax.size_term trm; 
          num_tags := ntags;
        end 
      else
        begin
          let dfa, nqrys = Automata.instrument tt in 
          nqueries := nqrys;
          let fdd = Profile.profile "[Extraction]" Automata.compile dfa in 
          let sz_t, sz_q = Syntax.size_breakdown tt in
          sz_trm := sz_t; 
          sz_qry := sz_q;
          let (rs, rs_opt1, rs_opt2, ntags) = Profile.profile "[Rule Generation]" Fdd.rules fdd in
          rules := rs_opt2;
          num_rules := List.length rs;
          num_rules_opt1 := List.length rs_opt1;
          num_rules_opt2 := List.length rs_opt2;
          num_tags := ntags;
        end);
      let total_time = Sys.time () -. start in

      (match !out_file with 
      | None -> () 
      | Some f -> 
          let oc = open_out f in
          Fdd.write_rules oc !rules);

      if !stats then
        let time = get_time Profile.time_map in
        Printf.printf "%d, %d, %d, %d, %d, %d, %d, %f, %s, %s, %s, %s, %s, %s, %s, "
          !sz_trm
          !sz_qry
          !nqueries
          !num_rules
          !num_rules_opt1
          !num_rules_opt2
          !num_tags
          total_time
          (time "[Temporal Automaton]")
          (time "[Policy Automaton]")
          (time "[Determinize]")
          (time "[Minimize]")
          (time "[Intersection]") 
          (time "[Extraction]")
          (time "[Rule Generation]")
      else ()
  end
  with
    | Arg.Bad msg -> Printf.printf "%s" msg
    | Arg.Help msg -> Printf.printf "%s" msg
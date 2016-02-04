open Common

let time_verbose descr f x =
  let t = Sys.time() in
  let res = f x in
  Printf.printf "%s: %f (sec)\n" descr (Sys.time() -. t);
  res

let time f x = 
  let t = Sys.time() in
  let res = f x in
  (res, Sys.time() -. t)

let time_map : (string, float) Hashtbl.t = Hashtbl.create 11

let add str secs = 
  try 
    let existing = Hashtbl.find time_map str in 
    Hashtbl.replace time_map str (existing +. secs)
  with Not_found -> 
	Hashtbl.add time_map str secs

let profile str f x = 
  let (res, secs) = time f x in 
  add str secs;
  res

let print_times () =
  let comparer (a,x) (b,y) = 
    let cmp = StrType.compare a b in 
    if cmp = 0 then compare x y 
    else cmp in
  let aux (s,t) = 
    print_endline (s ^ " ==> " ^ (string_of_float t)) in
  print_newline ();
  print_endline "Profiling Information:";
  print_endline "--------------------";
  let times = Hashtbl.fold (fun k v acc -> (k,v)::acc) time_map [] in
  let sorted = List.sort comparer times in
  List.iter aux sorted;
  print_endline "--------------------"

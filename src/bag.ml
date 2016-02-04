type 'a t = 
  | Empty
  | Leaf of 'a
  | Node of int * 'a t * 'a t

let length bag =
  match bag with 
  | Empty -> 0
  | Leaf v -> 1
  | Node (i,l,r) -> i

let empty = Empty

let singleton v = Leaf v

let append bag1 bag2 =
  match bag1, bag2 with 
  | Empty, _ -> bag2 
  | _, Empty -> bag1 
  | _, _ -> Node (length bag1 + length bag2, bag1, bag2)

let add e bag = append (singleton e) bag

let rec map f bag = 
  match bag with 
  | Empty -> Empty
  | Leaf v -> Leaf (f v)
  | Node(i,l,r) -> Node(i,map f l, map f r)

let rec fold f b bag = 
  match bag with 
  | Empty -> b 
  | Leaf v -> f b v 
  | Node(_,l,r) -> fold f (fold f b l) r

let rec iter f bag = 
  match bag with 
  | Empty -> () 
  | Leaf v -> f v 
  | Node(_,l,r) -> iter f l; iter f r

let rec first bag = 
  match bag with 
  | Empty -> raise (Failure "Bag.first")
  | Leaf v -> v 
  | Node (_,l,_) -> first l
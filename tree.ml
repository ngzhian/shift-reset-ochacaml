type tree_t = Empty
            | Node of tree_t * int * tree_t;;

type 'a result_t = Done
                 | Next of int * (unit / 'a -> 'a result_t / 'a);;

(* yield : int => unit *)
let yield n = shift (fun k -> Next (n, k));;

(* walk : tree_t => unit *)
let rec walk tree = match tree with
    Empty -> ()
  | Node (t1, n, t2) ->
      walk t1;
      yield n;
      walk t2;;

(* start : tree_t -> 'a result_t *)
let start tree =
  reset (fun () -> walk tree; Done);;

(* print_nodes : tree_t -> unit *)
let print_nodes tree =
  let rec loop r = match r with
      Done -> ()
    | Next (n, k) ->
      print_int n;
      loop (k ()) in
  loop (start tree);;

(* add_tree : tree_t -> int *)
let add_tree tree =
  let rec loop r = match r with
      Done -> 0
    | Next (n, k) -> n + (loop (k ())) in
  loop (start tree);;

(* same_fringe : tree_t -> tree_t -> bool *)
let same_fringe t1 t2 =
  let rec loop r1 r2 = match (r1, r2) with
      Done, Done -> true
    | Next _, Done -> false
    | Done, Next (n, k) -> false
    | Next (n1, k1), Next (n2, k2) ->
      n1 = n2 && loop (k1 ()) (k2 ())
  in
  loop (start t1) (start t2);;

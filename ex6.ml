(* Call this like:
 * let f = reset (fun () -> id [1;2;3]);;
 * f [4;5;6];;
 * > [1;2;3;4;5;6]
 *)
let rec id lst = match lst with
    [] -> shift (fun k -> k)
  | first :: rest -> first :: id rest ;;

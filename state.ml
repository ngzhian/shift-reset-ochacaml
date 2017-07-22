let get () =
  shift (fun k -> fun state -> k state state);;

let tick () =
  shift (fun k -> fun state -> k () (state + 1));;

let run_state thunk =
  reset (fun () -> let result = thunk () in
          fun state -> result) 0;;

let put x =
  shift (fun k -> fun state -> k () x);;

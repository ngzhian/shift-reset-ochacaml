let rec times lst = match lst with
  | [] -> 1
  | 0 :: rest -> shift (fun _ -> 0)
  | first::rest -> first * times rest

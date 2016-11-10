(* mutually recursion *)
let rec odd x =
  if x <= 0 then false else even (x - 1)
and even x =
  if x <= 0 then true else odd (x - 1)

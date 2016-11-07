let f = function
    `Int x -> (float_of_int x)
  | `Float x -> x

let () =
  let x = `Float 3.2 in
  Printf.printf "%f \n" (f x)

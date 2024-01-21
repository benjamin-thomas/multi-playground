(* 31 - Determine whether a given integer number is prime *)

(* Brute force approach *)
let is_prime n =
  let rec not_div_by x y =
    match y with
    | 1 -> true
    | _ -> x mod y <> 0 && not_div_by x (y - 1)
  in

  n > 1 && not_div_by n (n - 1)
;;

let%test _ = not @@ is_prime 1
let%test _ = is_prime 2
let%test _ = is_prime 3
let%test _ = not @@ is_prime 4
let%test _ = is_prime 5
let%test _ = not @@ is_prime 6
let%test _ = is_prime 7
let%test _ = not @@ is_prime 8
let%test _ = not @@ is_prime 9
let%test _ = not @@ is_prime 10
let%test _ = is_prime 11
let%test _ = not @@ is_prime 12

open Printf

module Check = struct
  let positive_number n =
    if n > 0 then
      Ok n
    else
      Error (sprintf "Not positive: %d" n)
  ;;
end

module SequentialValidation = struct
  (* Result.map2 *)
  let map2 func ra rb =
    match ra with
    | Error xa -> Error xa
    | Ok a ->
      (match rb with
       | Error xb -> Error xb
       | Ok b -> Ok (func a b))
  ;;

  (* arg is required for proper type inference *)
  let apply arg = map2 (fun x f -> f x) arg

  (*
     Apply could also be defined that way.
     But using [map2] creates an intresting parallel between sequential and parallel validations.

     let apply mx mf =
     match (mx, mf) with
     | (Ok x, Ok f) -> Ok (f x)
     | (Error a, _) -> Error a
     | (_, Error b) -> Error b
     ;;
  *)

  let add a b c =
    Ok (fun x y z -> x + y + z)
    |> apply (Check.positive_number a)
    |> apply (Check.positive_number b)
    |> apply (Check.positive_number c)
  [@@ocamlformat "disable"]

  let%test_unit "sequential validation" =
    (* Validates from the right! *)
    let open Base in
    let test = [%test_result: (int, string) Result.t] in
    ()
    ; test (add 1 2 3) ~expect:(Ok 6)
    ; test (add 0 2 3) ~expect:(Error "Not positive: 0")
    ; test (add 1 (-2) 3) ~expect:(Error "Not positive: -2")
    ; test (add 1 (-2) (-3)) ~expect:(Error "Not positive: -3")
  ;;
end

module ParallelValidation = struct
  let map2 f ra rb =
    match (ra, rb) with
    | (Error ea, Error eb) -> Error (ea @ eb)
    | (Error ea, Ok _) -> Error ea
    | (Ok _, Error eb) -> Error eb
    | (Ok a, Ok b) -> Ok (f a b)
  ;;

  let apply arg = map2 (fun x f -> f x) (Result.map_error (fun err -> [ err ]) arg)

  let add a b c =
    Ok (fun x y z -> x + y + z)
    |> apply (Check.positive_number a)
    |> apply (Check.positive_number b)
    |> apply (Check.positive_number c)
  [@@ocamlformat "disable"]

  let%test_unit "parallel validation" =
    (* Validates from the right! *)
    let open Base in
    let test = [%test_result: (int, string list) Result.t] in
    ()
    ; test (add 1 2 3) ~expect:(Ok 6)
    ; test (add 0 2 3) ~expect:(Error [ "Not positive: 0" ])
    ; test (add 1 (-2) 3) ~expect:(Error [ "Not positive: -2" ])
    ; test (add 1 (-2) (-3)) ~expect:(Error [ "Not positive: -3"; "Not positive: -2" ])
  ;;
end

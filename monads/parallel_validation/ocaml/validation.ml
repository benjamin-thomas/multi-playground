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

let option_to_list f value =
  match f value with
  | None -> []
  | Some x -> [ x ]
;;

module ParallelValidation2 = struct
  open Base

  type ('err, 'raw) validation = 'raw -> 'err option
  type email = Email of string [@@deriving sexp, compare]

  let mkEmail str = Email str

  let validate constructor validations value =
    match List.concat_map ~f:(fun f -> option_to_list f value) validations with
    | [] -> Ok (constructor value)
    | errs -> Error errs
  ;;

  let length_greater_than n err v =
    if String.length v < n then
      Some err
    else
      None
  ;;

  let string_includes pattern err v =
    if String.is_substring v ~substring:pattern then
      None
    else
      Some err
  ;;

  let%test_unit "parallel validation 2" =
    let open Base in
    let test = [%test_result: string option] in
    ()
    ; test ~expect:None (length_greater_than 5 "too short!" "hello")
    ; test ~expect:(Some "too short!") (length_greater_than 5 "too short!" "wat")
  ;;

  let%test_unit "validate" =
    let email_validators =
      [ length_greater_than 5 "too short!"
      ; string_includes "@" "does not look like an email"
      ]
    in
    let open Base in
    let test = [%test_result: (email, string list) Result.t] in
    ()
    ; test
        ~expect:(Error [ "too short!"; "does not look like an email" ])
        (validate mkEmail email_validators "user")
    ; test
        ~expect:(Ok (Email "user@example.com"))
        (validate mkEmail email_validators "user@example.com")
  ;;
end

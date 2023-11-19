(*
   dune runtest -w

   Per: https://github.com/xvw/preface/blob/master/guides/option_instantiation.md
*)

(* Without `ppx_assert` *)
let%test _ = 1 + 2 = 3
(* let%test_unit _ = print_endline "print OK, or use failwith" *)

let%test_module _ =
  (module struct
    (* let%test_unit _ = print_endline "print OK2, or use failwith" *)
    let%test _ = 1 + 2 = 3
  end)
;;

(* With `ppx_assert` *)
let%test_unit _ = [%test_eq: Base.int] (1 + 2) 3
let%test_unit _ = [%test_result: Base.int] (List.length [ 1; 2 ]) ~expect:2

let%test_module _ =
  (module struct
    let%test_unit _ = [%test_eq: Base.int] (1 + 2) 3
    let%test_unit _ = [%test_result: Base.int] (List.length [ 1; 2 ]) ~expect:2
  end)
;;

module Opt_monad = Preface.Make.Monad.Via_return_and_bind (struct
    type 'a t = 'a option

    let return x = Some x

    let bind f = function
      | None -> None
      | Some x -> f x
    ;;
  end)

module Opt_monad2 = Preface.Make.Monad.Via_return_map_and_join (struct
    type 'a t = 'a option

    let return x = Some x

    let map f = function
      | None -> None
      | Some x -> Some (f x)
    ;;

    let join = function
      | None -> None
      | Some x -> x
    ;;
  end)

let%test_unit _ =
  let ( => ) = [%test_eq: Base.int] in
  let f = List.length in
  ()
  ; 2 => f [ 1; 2 ]
  ; 3 => f [ 1; 2; 3 ]
;;

[@@@ocamlformat "disable"]
let res1 =
  let open Opt_monad in
  return      1  >>= fun x ->
  return (x + 2) >>= fun y ->
  return (y * 2)
;;
[@@@ocamlformat "enable"]

let res2 =
  let open Opt_monad in
  let* x = return 1 in
  let* y = return (x + 2) in
  return (y * 2)
;;

let res3 =
  let open Opt_monad2 in
  let* x = return 1 in
  let* y = return (x + 2) in
  return (y * 2)
;;

let%test_unit _ =
  let ( => ) =
    let open Base in
    [%test_eq: int option]
  in
  ()
  ; res1 => Some 6
  ; res2 => Some 6
  ; res3 => Some 6
;;

module Opt_core = struct
  include Preface.Make.Monad.Core_via_return_and_bind (struct
      type 'a t = 'a option

      let return x = Some x

      let bind f = function
        | None -> None
        | Some x -> f x
      ;;
    end)

  let map f x =
    let () = print_endline "custom map was injected (for perf)" in
    match x with
    | None -> None
    | Some y -> Some (f y)
  ;;
end

module Opt_op = Preface.Make.Monad.Operation (Opt_core)
module Opt_infx = Preface.Make.Monad.Infix (Opt_core) (Opt_op)
module Opt_syn = Preface.Make.Monad.Syntax (Opt_core)

module Opt_with_custom_map =
  Preface.Make.Monad.Via (Opt_core) (Opt_op) (Opt_infx) (Opt_syn)

let%expect_test _ =
  let print = function
    | None -> print_string "NONE"
    | Some x -> Printf.printf "Some(%d)" x
  in
  ()
  ; print Opt_with_custom_map.(succ =|< return 1)
  ; [%expect {|
    custom map was injected (for perf)
    Some(2)|}]
  ; ()
  ; print Opt_with_custom_map.(return 2 >|= succ)
  ; [%expect {|
    custom map was injected (for perf)
    Some(3) |}]
  ; ()
  ; print Opt_with_custom_map.(succ <$> return 3)
  ; [%expect {|
    custom map was injected (for perf)
    Some(4) |}]
  ; ()
  ; print Opt_with_custom_map.(return 4 <&> succ);
  [%expect {|
    custom map was injected (for perf)
    Some(5) |}]
;;

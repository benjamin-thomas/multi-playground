module To_test = struct
  let lowercase = String.lowercase_ascii
  let capitalize = String.capitalize_ascii
  let str_concat = String.concat ""
  let list_concat = List.append
end

(* The tests *)
let test_lowercase () =
  Alcotest.(check string) "same string" "hello!" (To_test.lowercase "hELLO!")
;;

let test_capitalize () =
  Alcotest.(check string) "same string" "World." (To_test.capitalize "world.")
;;

let test_str_concat () =
  Alcotest.(check string) "same string" "foobar" (To_test.str_concat [ "foo"; "bar" ])
;;

let test_list_concat () =
  Alcotest.(check (list int))
    "same lists"
    [ 1; 2; 3 ]
    (To_test.list_concat [ 1 ] [ 2; 3 ])
;;

module Author = struct
  type t =
    { id : int
    ; first_name : string
    ; last_name : string
    }

  let pp ppf { id; first_name; last_name } =
    (* Fmt.pf ppf "[%d|%s|%s]" id first_name last_name *)
    Fmt.pf ppf "{ id = %d; first_name = %s; last_name = %s }" id first_name last_name
  ;;

  (* let equal x y = x = y *)
end

let test_author () =
  let author = Alcotest.testable Author.pp ( = ) in
  Alcotest.(check author)
    "Author is equal"
    { id = 1; first_name = "John"; last_name = "Doe" }
    { id = 1; first_name = "John"; last_name = "Doe" }
;;

let tests =
  [ ( "string-case"
    , [ Alcotest.test_case "Lower case" `Quick test_lowercase
      ; Alcotest.test_case "Capitalization" `Quick test_capitalize
      ] )
  ; ("string-concat", [ Alcotest.test_case "String mashing" `Quick test_str_concat ])
  ; ("list-concat", [ Alcotest.test_case "List mashing" `Slow test_list_concat ])
  ; ("author", [ Alcotest.test_case "Author" `Quick test_author ])
  ]
;;

(* Run it *)
let () = Alcotest.run "Utils" (tests @ Other.tests)

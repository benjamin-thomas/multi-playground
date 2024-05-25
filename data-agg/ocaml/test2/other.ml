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

let test_author_eq () =
  (* Using poly compare, that's just fine here. *)
  let author = Alcotest.testable Author.pp ( = ) in
  Alcotest.(check author)
    "Author is equal"
    { id = 1; first_name = "John"; last_name = "Doe" }
    { id = 1; first_name = "John"; last_name = "Doe" }
;;

(** represents a row of joined data *)
type row =
  { author_id : int
  ; author_fname : string
  ; author_lname : string
  ; book_id : int
  ; book_title : string
  ; isbn : string
  }

let rows =
  [ { author_id = 111
    ; author_fname = "John"
    ; author_lname = "Whitington"
    ; book_id = 111
    ; book_title = "OCaml from the Very Beginning"
    ; isbn = "111-111"
    }
  ; { author_id = 111
    ; author_fname = "John"
    ; author_lname = "Whitington"
    ; book_id = 222
    ; book_title = "More OCaml"
    ; isbn = "111-222"
    }
  ; { author_id = 222
    ; author_fname = "Graham"
    ; author_lname = "Hutton"
    ; book_id = 333
    ; book_title = "Programming in Haskell"
    ; isbn = "222-333"
    }
  ; { author_id = 333
    ; author_fname = "Yaron"
    ; author_lname = "Minsky"
    ; book_id = 444
    ; book_title = "Real World OCaml"
    ; isbn = "333-444+555"
    }
  ; { author_id = 444
    ; author_fname = "Anil"
    ; author_lname = "Madhavapeddy"
    ; book_id = 444
    ; book_title = "Real World OCaml"
    ; isbn = "333-444+555"
    }
  ]
;;

module IntMap = Map.Make (Int)

(** represents the final tree-like structure, from the author's perspective *)
module Author_view = struct
  type author =
    { id : int
    ; first_name : string
    ; last_name : string
    }

  type book =
    { id : int
    ; title : string
    ; isbn : string
    }

  type t =
    { author : author
    ; books : book list
    }

  let author_pp ppf (x : author) =
    Fmt.pf
      ppf
      "{ id = %d; first_name = %s; last_name = %s }"
      x.id
      x.first_name
      x.last_name
  ;;

  let book_pp ppf (x : book) =
    Fmt.pf ppf "{ id = %d; title = %s; isbn = %s }" x.id x.title x.isbn
  ;;

  let pp ppf x =
    Fmt.pf ppf "{ author = %a; books = %a }" author_pp x.author (Fmt.list book_pp) x.books
  ;;

  let of_rows rows =
    let init = IntMap.empty in
    let author_map =
      List.fold_left
        (fun acc row ->
          let book = { id = row.book_id; title = row.book_title; isbn = row.isbn } in
          IntMap.update
            row.author_id
            (function
              | None ->
                Some
                  { author =
                      { id = row.author_id
                      ; first_name = row.author_fname
                      ; last_name = row.author_lname
                      }
                  ; books = [ book ]
                  }
              | Some author_view ->
                Some { author_view with books = book :: author_view.books })
            acc)
        init
        rows
    in
    (* Back to a list *)
    let res = List.rev @@ IntMap.fold (fun _ v acc -> v :: acc) author_map [] in
    (* Below is not strictly necessary *)
    let sort_books author = { author with books = List.sort compare author.books } in
    List.map sort_books res
  ;;
end

let test_author_view () =
  let author_view = Alcotest.testable (Fmt.list Author_view.pp) ( = ) in
  let want : Author_view.t list =
    [ { author = { id = 111; first_name = "John"; last_name = "Whitington" }
      ; books =
          [ { id = 111; title = "OCaml from the Very Beginning"; isbn = "111-111" }
          ; { id = 222; title = "More OCaml"; isbn = "111-222" }
          ]
      }
    ; { author = { id = 222; first_name = "Graham"; last_name = "Hutton" }
      ; books = [ { id = 333; title = "Programming in Haskell"; isbn = "222-333" } ]
      }
    ; { author = { id = 333; first_name = "Yaron"; last_name = "Minsky" }
      ; books = [ { id = 444; title = "Real World OCaml"; isbn = "333-444+555" } ]
      }
    ; { author = { id = 444; first_name = "Anil"; last_name = "Madhavapeddy" }
      ; books = [ { id = 444; title = "Real World OCaml"; isbn = "333-444+555" } ]
      }
    ]
  in
  let got = Author_view.of_rows rows in
  Alcotest.(check author_view) "Author_view is equal" want got
;;

let tests =
  [ ("author_eq", [ Alcotest.test_case "Author smoke test" `Quick test_author_eq ])
  ; ("author_view", [ Alcotest.test_case "Author_view" `Quick test_author_view ])
  ]
;;

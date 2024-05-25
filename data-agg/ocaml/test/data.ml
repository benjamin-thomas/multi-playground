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

module Author = struct
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

  let compare (a : t) (b : t) = Int.compare a.author.id b.author.id
end

(* module AuthorMap = Map.Make (Author) *)
module AuthorMap = Map.Make (Int)
module IntMap = Map.Make (Int)

(** represents the final tree-like structure, from the author's perspective *)
module Author_view = struct
  open Base

  type author =
    { id : int
    ; first_name : string
    ; last_name : string
    }
  [@@deriving eq, ord, sexp_of]

  type book =
    { id : int
    ; title : string
    ; isbn : string
    }
  [@@deriving eq, ord, sexp_of]

  type t =
    { author : author
    ; books : book list
    }
  [@@deriving eq, ord, sexp_of]

  let of_rows rows =
    let init = Map.empty (module Int) in
    let author_map =
      List.fold rows ~init ~f:(fun acc row ->
        let book = { id = row.book_id; title = row.book_title; isbn = row.isbn } in
        Map.update acc row.author_id ~f:(function
          | None ->
            { author =
                { id = row.author_id
                ; first_name = row.author_fname
                ; last_name = row.author_lname
                }
            ; books = [ book ]
            }
          | Some author_view -> { author_view with books = book :: author_view.books }))
    in
    let res = author_map |> Map.to_alist |> List.map ~f:snd in
    let sort_books author =
      { author with books = List.sort author.books ~compare:compare_book }
    in
    List.map res ~f:sort_books
  ;;

  let of_rows2 rows =
    let init = IntMap.empty in
    let author_map =
      List.fold_left rows ~init ~f:(fun acc row ->
        let book = { id = row.book_id; title = row.book_title; isbn = row.isbn } in
        AuthorMap.update
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
    in
    let res = AuthorMap.fold (fun _ v acc -> v :: acc) author_map [] in
    let sort_books author =
      { author with
        books = List.sort ~compare:(fun b1 b2 -> Int.compare b1.id b2.id) author.books
      }
    in
    List.map ~f:sort_books res |> List.rev
  ;;
end

let%test_unit _ =
  let want_john : Author_view.t =
    { author = { id = 111; first_name = "John"; last_name = "Whitington" }
    ; books =
        [ { id = 111; title = "OCaml from the Very Beginning"; isbn = "111-111" }
        ; { id = 222; title = "More OCaml"; isbn = "111-222" }
        ]
    }
  in
  let want_paul : Author_view.t =
    { author = { id = 222; first_name = "Graham"; last_name = "Hutton" }
    ; books = [ { id = 333; title = "Programming in Haskell"; isbn = "222-333" } ]
    }
  in
  let want_yaron : Author_view.t =
    { author = { id = 333; first_name = "Yaron"; last_name = "Minsky" }
    ; books = [ { id = 444; title = "Real World OCaml"; isbn = "333-444+555" } ]
    }
  in
  let want_anil : Author_view.t =
    { author = { id = 444; first_name = "Anil"; last_name = "Madhavapeddy" }
    ; books = [ { id = 444; title = "Real World OCaml"; isbn = "333-444+555" } ]
    }
  in
  let want_all : Author_view.t list = [ want_john; want_paul; want_yaron; want_anil ] in
  let got_all = Author_view.of_rows rows in
  let got_all2 = Author_view.of_rows2 rows in
  let got_john =
    List.find_opt
      (fun (x : Author_view.t) ->
        x.author.first_name = "John" && x.author.last_name = "Whitington")
      got_all
  in
  ()
  ; [%test_eq: Author_view.t Base.option] (Some want_john) got_john
  ; [%test_eq: Author_view.t Base.List.t] want_all got_all
  ; [%test_eq: Author_view.t Base.List.t] want_all got_all2
;;

(*
   Using expect tests is probably the best option for now.
   It provides some kind of color diff when the test fails (whereas Alcotest doesn't)
   The only downside is that the lisp-like expected output is a bit difficult to parse, visually.
*)
let%expect_test _ =
  let got_all = Author_view.of_rows rows in
  let got_all2 = Author_view.of_rows2 rows in
  let got_john =
    List.find_opt
      (fun (x : Author_view.t) ->
        x.author.first_name = "John" && x.author.last_name = "Whitington")
      got_all
  in
  let open Core in
  ()
  ; print_s [%sexp (got_john : Author_view.t Base.option)]
  ; [%expect
      {|
    (((author ((id 111) (first_name John) (last_name Whitington)))
      (books
       (((id 111) (title "OCaml from the Very Beginning") (isbn 111-111))
        ((id 222) (title "More OCaml") (isbn 111-222)))))) |}]
  ; ()
  ; print_s [%sexp (got_all : Author_view.t Base.List.t)]
  ; [%expect
      {|
    (((author ((id 111) (first_name John) (last_name Whitington)))
      (books
       (((id 111) (title "OCaml from the Very Beginning") (isbn 111-111))
        ((id 222) (title "More OCaml") (isbn 111-222)))))
     ((author ((id 222) (first_name Graham) (last_name Hutton)))
      (books (((id 333) (title "Programming in Haskell") (isbn 222-333)))))
     ((author ((id 333) (first_name Yaron) (last_name Minsky)))
      (books (((id 444) (title "Real World OCaml") (isbn 333-444+555)))))
     ((author ((id 444) (first_name Anil) (last_name Madhavapeddy)))
      (books (((id 444) (title "Real World OCaml") (isbn 333-444+555)))))) |}]
  ; ()
  ; print_s [%sexp (got_all2 : Author_view.t Base.List.t)]
  ; [%expect
      {|
    (((author ((id 111) (first_name John) (last_name Whitington)))
      (books
       (((id 111) (title "OCaml from the Very Beginning") (isbn 111-111))
        ((id 222) (title "More OCaml") (isbn 111-222)))))
     ((author ((id 222) (first_name Graham) (last_name Hutton)))
      (books (((id 333) (title "Programming in Haskell") (isbn 222-333)))))
     ((author ((id 333) (first_name Yaron) (last_name Minsky)))
      (books (((id 444) (title "Real World OCaml") (isbn 333-444+555)))))
     ((author ((id 444) (first_name Anil) (last_name Madhavapeddy)))
      (books (((id 444) (title "Real World OCaml") (isbn 333-444+555)))))) |}]
;;

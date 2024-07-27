(*
   dune exec --no-print-directory --display=quiet ./main.exe -w
*)

module DH = Dream_html
module H = DH.HTML
module HX = DH.Hx

let page _req =
  H.html
    [ H.lang "en" ]
    [ H.head [] [ H.title [] "Dream-html" ]
    ; H.body
        [ HX.boost true ]
        [ H.h1 [] [ DH.txt "Dream-html" ]
        ; H.p [] [ DH.txt "Is cool!" ]
        ; H.form
            [ H.method_ `POST; H.action "/feedback" ]
            [ (* Integrated with Dream's CSRF token generation *)
              (* DH.csrf_tag req *)
              H.label [ H.for_ "what-you-think" ] [ DH.txt "Tell us what you think!" ]
            ; H.input [ H.name "what-you-think"; H.id "what-you-think" ]
            ; H.input [ H.type_ "submit"; H.value "Send" ]
            ]
        ]
    ]
;;

let () =
  let req =
    (* Create a dummy request or get it from Dream context *)
    Dream.request ~method_:`GET "/"
    (* Dream.request "hello" *)
  in
  let rendered = Format.asprintf "%a" DH.pp (page req) in
  (* let req : Dream.request = Dream.request "hello" in *)
  (* let rendered = DH.pp @@ page req in *)
  print_endline rendered
;;

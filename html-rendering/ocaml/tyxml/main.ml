(*
   dune exec --no-print-directory --display=quiet ./main.exe -w
*)

module H = Tyxml.Html

(* dune exec --no-print-directory --display=quiet ./main.exe -w *)

let mycontent =
  H.div
    ~a:[ H.a_class [ "content" ] ]
    [ H.h1 [ H.txt "A fabulous title" ]; H.txt "This is a fabulous content." ]
;;

let mytitle = H.title (H.txt "A Fabulous Web Page")
let mypage = H.html (H.head mytitle []) (H.body [ mycontent ])

let () =
  let fmt = Format.formatter_of_out_channel stdout in
  Format.fprintf fmt "%a@." (H.pp ~indent:true ()) mypage
;;

open Tyxml

(* dune exec --no-print-directory --display=quiet ./main_ppx.exe -w *)

let%html mycontent =
  {|
  <div class="content">
    <h1>A fabulous title</h1>
    This is a fabulous content.
  </div>
|}
;;

let mytitle = Html.txt "A Fabulous Web Page"

let%html mypage =
  {|<html>
     <head>
       <title>|}
    mytitle
    {|</title>
     </head>
     <body>|}
    [ mycontent ]
    {|</body>
   </html>
  |}
;;

let () =
  let fmt = Format.formatter_of_out_channel stdout in
  Format.fprintf fmt "%a@." (Html.pp ~indent:true ()) mypage
;;

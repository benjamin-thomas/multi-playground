(*
curl --silent -X 'GET' \
  "https://api.laposte.fr/suivi/v2/idships/$TRACKING?lang=fr_FR" \
  -H 'accept: application/json' \
  -H "X-Okapi-Key: $API_KEY"

  dune exec --display=quiet ./bin/main.exe 5Z00506074154
*)

module U = Yojson.Safe.Util
module P = Printf

let setup () =
  let ( let* ) = Option.bind in
  let* api_key = Sys.getenv_opt "API_KEY" in
  let* tracking_number =
    try Some Sys.argv.(1) with
    | _ -> None
  in
  Some (api_key, tracking_number)
;;

let usage = {|
Usage:

API_KEY=123 ./main.exe TRACKING_NUMBER
|}

let () =
  match setup () with
  | None -> print_endline usage
  | Some (api_key, tracking_number) -> (
      let fetch = Colissimo.Api.fetch_tracking api_key in
      match Lwt_main.run (fetch tracking_number) with
      | Error x -> print_endline @@ "Fetch tracking error: " ^ x
      | Ok (status_code, delivery_status, pretty_src) ->
          ()
          ; print_endline pretty_src
          ; print_endline "---"
          ; print_endline @@
              Colissimo.To_string.text_summary
                status_code
                tracking_number
                delivery_status
  )
[@@ocamlformat "disable"]

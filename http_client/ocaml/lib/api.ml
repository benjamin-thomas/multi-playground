(* == HTTP == *)

(* Cohttp *)
module Code = Cohttp.Code
module Header = Cohttp.Header

(* Cohttp_lwt_unix *)
module Client = Cohttp_lwt_unix.Client
module Response = Cohttp_lwt_unix.Response

let with_timout ~time ~f =
  let open Lwt.Infix in
  Lwt.pick
    [ (f () >|= fun v -> `Done v)
    ; (Lwt_unix.sleep time >|= fun () -> `Timeout)
    ]
;;

(* == CALL API == *)

let extract_body status_code body =
  let ( >>= ) = Lwt.bind in
  body |> Cohttp_lwt.Body.to_string >>= fun body_s ->
  match Json.parse_json body_s with
  | Error s -> Lwt.return (Error ("JSON decode error: " ^ s))
  | Ok (resp, pretty) -> Lwt.return (Ok (status_code, resp, pretty))
;;

let fetch_tracking api_key tracking =
  let ( >>= ) = Lwt.bind in
  let ( >|= ) = Lwt.map |> Fun.flip in
  let url =
    Printf.sprintf "https://api.laposte.fr/suivi/v2/idships/%s?lang=fr_FR"
      tracking
  in
  let uri = Uri.of_string url in
  let headers =
    Header.of_list [ ("accept", "application/json"); ("X-Okapi-Key", api_key) ]
  in
  let fetch =
    with_timout ~time:2.0 ~f:(fun () -> Client.get uri ~headers) >|= function
    | `Timeout -> Error "Timeout!"
    | `Done (resp, body) -> Ok (resp, body)
  in
  fetch >>= function
  | Error err -> Lwt.return (Error err)
  | Ok (resp, body) -> (
      let status_code = resp |> Response.status |> Code.code_of_status in
      match status_code with
      | 404 -> extract_body status_code body (* wat! *)
      | 200 -> extract_body status_code body
      | _ ->
          let msg = Printf.sprintf "Request returned status: %d" status_code in
          Lwt.return (Error msg))
;;

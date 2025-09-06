open Lwt.Syntax

(* OCaml SSE server using cohttp-lwt-unix for better disconnection detection *)

let port = 5007

(* SSE Client data structure *)
type sse_client =
  { id : string
  ; connected_at : float
  ; mutable active : bool
  }

let clients = Hashtbl.create 16
let client_counter = ref 0
let clients_mutex = Lwt_mutex.create ()

(* Generate alphabetic IDs *)
let generate_alphabetic_id n =
  let rec go num acc =
    if num < 0
    then acc
    else (
      let remainder = num mod 26 in
      let quotient = num / 26 in
      let char = String.make 1 (char_of_int (65 + remainder)) in
      if quotient = 0 then char ^ acc else go (quotient - 1) (char ^ acc))
  in
  go n ""
;;

(* Log with timestamp and flush *)
let log_message msg =
  let timestamp =
    Unix.time ()
    |> Unix.gmtime
    |> fun tm ->
    Printf.sprintf
      "%04d-%02d-%02dT%02d:%02d:%02d.000Z"
      (tm.tm_year + 1900)
      (tm.tm_mon + 1)
      tm.tm_mday
      tm.tm_hour
      tm.tm_min
      tm.tm_sec
  in
  Printf.printf "[%s] %s\n" timestamp msg;
  flush_all ()
;;

(* Cleanup client *)
let cleanup_client client_id =
  let* () = Lwt_mutex.lock clients_mutex in
  (match Hashtbl.find_opt clients client_id with
   | Some client ->
     client.active <- false;
     Hashtbl.remove clients client_id;
     log_message
       (Printf.sprintf
          "Client %s removed. Remaining: %d"
          client_id
          (Hashtbl.length clients))
   | None -> ());
  Lwt_mutex.unlock clients_mutex;
  Lwt.return_unit
;;

(* Handle SSE endpoint *)
let handle_sse _req _body =
  let client_id = generate_alphabetic_id !client_counter in
  incr client_counter;
  log_message (Printf.sprintf "Client %s connecting..." client_id);
  let connected_at = Unix.time () *. 1000.0 in
  let client = { id = client_id; connected_at; active = true } in
  (* Add to clients *)
  let* () = Lwt_mutex.lock clients_mutex in
  Hashtbl.add clients client_id client;
  let total_clients = Hashtbl.length clients in
  Lwt_mutex.unlock clients_mutex;
  log_message
    (Printf.sprintf "Client %s connected. Total clients: %d" client_id total_clients);
  (* Create SSE stream *)
  let stream, push = Lwt_stream.create () in
  (* Send initial message *)
  let initial_json =
    Printf.sprintf
      "{\"type\":\"connected\",\"clientId\":\"%s\",\"message\":\"OCaml/cohttp SSE \
       connection established\"}"
      client_id
  in
  let initial_msg = Printf.sprintf "data: %s\n\n" initial_json in
  push (Some initial_msg);
  (* Start ping loop in background *)
  let rec ping_loop () =
    let* () = Lwt_unix.sleep 1.0 in
    if not client.active
    then Lwt.return_unit
    else (
      let timestamp = Unix.time () *. 1000.0 in
      let* () = Lwt_mutex.lock clients_mutex in
      let total_clients = Hashtbl.length clients in
      Lwt_mutex.unlock clients_mutex;
      let ping_json =
        Printf.sprintf
          "{\"type\":\"ping\",\"timestamp\":%.0f,\"clientId\":\"%s\",\"totalClients\":%d}"
          timestamp
          client_id
          total_clients
      in
      let ping_msg = Printf.sprintf "data: %s\n\n" ping_json in
      Lwt.catch
        (fun () ->
           push (Some ping_msg);
           log_message (Printf.sprintf "Ping sent to %s" client_id);
           ping_loop ())
        (fun exn ->
           log_message
             (Printf.sprintf
                "Error sending ping to %s: %s"
                client_id
                (Printexc.to_string exn));
           let* () = cleanup_client client_id in
           push None;
           (* Close stream *)
           Lwt.return_unit))
  in
  (* Start ping loop async *)
  Lwt.async (fun () -> ping_loop ());
  (* Create streaming body *)
  let body = Cohttp_lwt.Body.of_stream stream in
  (* Return SSE response *)
  let headers =
    Cohttp.Header.of_list
      [ "Content-Type", "text/event-stream"
      ; "Cache-Control", "no-cache"
      ; "Connection", "keep-alive"
      ; "Access-Control-Allow-Origin", "*"
      ]
  in
  Cohttp_lwt_unix.Server.respond ~headers ~status:`OK ~body ()
;;

(* Handle status endpoint *)
let handle_status _req _body =
  let* () = Lwt_mutex.lock clients_mutex in
  let clients_list = Hashtbl.fold (fun _ client acc -> client :: acc) clients [] in
  let total_clients = List.length clients_list in
  Lwt_mutex.unlock clients_mutex;
  let clients_json =
    clients_list
    |> List.mapi (fun i client ->
      let comma = if i = List.length clients_list - 1 then "" else "," in
      Printf.sprintf
        "    {\"id\": \"%s\", \"connectedAt\": %.0f}%s"
        client.id
        client.connected_at
        comma)
    |> String.concat "\n"
  in
  let status_json =
    Printf.sprintf
      {|{
  "activeConnections": %d,
  "clients": [
%s
  ],
  "implementation": "OCaml/cohttp (better disconnection detection)"
}|}
      total_clients
      clients_json
  in
  let headers = Cohttp.Header.of_list [ "Content-Type", "application/json" ] in
  Cohttp_lwt_unix.Server.respond_string ~headers ~status:`OK ~body:status_json ()
;;

(* Main request handler *)
let handler _conn req body =
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  match Cohttp.Request.meth req, path with
  | `GET, "/events" -> handle_sse req body
  | `GET, "/status" -> handle_status req body
  | `OPTIONS, _ ->
    let headers =
      Cohttp.Header.of_list
        [ "Access-Control-Allow-Origin", "*"
        ; "Access-Control-Allow-Methods", "GET, OPTIONS"
        ; "Access-Control-Allow-Headers", "Content-Type"
        ]
    in
    Cohttp_lwt_unix.Server.respond_string ~headers ~status:`OK ~body:"" ()
  | _ ->
    Cohttp_lwt_unix.Server.respond_string
      ~status:`Not_found
      ~body:"Not Found\n\nAvailable endpoints:\n- /events (SSE)\n- /status (JSON)"
      ()
;;

let start_server port =
  Printf.printf "\n";
  Printf.printf "🟤 OCaml/cohttp SSE Server Starting (Port %d)\n" port;
  Printf.printf "✅ Testing cohttp for better disconnection detection:\n";
  Printf.printf "   - Using exception handling to detect disconnections\n";
  Printf.printf "   - Thread-safe client management with mutexes\n";
  Printf.printf "\n";
  Printf.printf "Expected behavior: Better disconnection detection than Dream\n";
  Printf.printf "\n";
  Printf.printf "Endpoints:\n";
  Printf.printf "  - http://localhost:%d/events (SSE stream)\n" port;
  Printf.printf "  - http://localhost:%d/status (Active connections)\n" port;
  flush_all ();
  let callback _conn req body = handler _conn req body in
  Cohttp_lwt_unix.Server.create
    ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback ())
;;

let () = Lwt_main.run (start_server port)

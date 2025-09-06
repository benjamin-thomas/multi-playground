(* OCaml SSE server using Eio for immediate disconnection detection *)
(* Effects-based direct-style IO for better socket control *)

let port = 5007

(* SSE Client data structure *)
type sse_client =
  { id : string
  ; connected_at : float
  ; mutable active : bool
  ; mutable last_ping : float
  }

let clients = Hashtbl.create 16
let client_counter = ref 0
let clients_mutex = Mutex.create ()

(* Generate alphabetic IDs (A, B, C, ..., Z, AA, AB, ...) *)
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

(* Log message with timestamp *)
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
  Mutex.lock clients_mutex;
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
  Mutex.unlock clients_mutex
;;

(* Parse simple HTTP request *)
let parse_http_request flow =
  try
    let buf_read = Eio.Buf_read.of_flow ~initial_size:1024 ~max_size:16384 flow in
    (* Read the request line *)
    let request_line = Eio.Buf_read.line buf_read in
    (* Skip headers by reading until empty line *)
    let rec skip_headers () =
      match Eio.Buf_read.line buf_read with
      | "" -> () (* Empty line marks end of headers *)
      | _ -> skip_headers () (* Skip this header line *)
    in
    skip_headers ();
    (* Parse request line *)
    let parts = String.split_on_char ' ' request_line in
    match parts with
    | [ method_name; path; _version ] -> Some (method_name, String.trim path)
    | _ -> None
  with
  | _ -> None
;;

(* Send HTTP response headers *)
let send_http_headers flow content_type additional_headers =
  let response = Buffer.create 256 in
  Buffer.add_string response "HTTP/1.1 200 OK\r\n";
  Buffer.add_string response (Printf.sprintf "Content-Type: %s\r\n" content_type);
  Buffer.add_string response "Cache-Control: no-cache\r\n";
  Buffer.add_string response "Connection: keep-alive\r\n";
  Buffer.add_string response "Access-Control-Allow-Origin: *\r\n";
  List.iter
    (fun (name, value) ->
       Buffer.add_string response (Printf.sprintf "%s: %s\r\n" name value))
    additional_headers;
  Buffer.add_string response "\r\n";
  Eio.Flow.copy_string (Buffer.contents response) flow
;;

(* Send SSE event *)
let send_sse_event flow event_type client_id message timestamp total_clients =
  let json_data =
    Printf.sprintf
      "data: \
       {\"type\":\"%s\",\"clientId\":\"%s\",\"message\":\"%s\",\"timestamp\":%.0f,\"totalClients\":%d}\n\n"
      event_type
      client_id
      message
      timestamp
      total_clients
  in
  Eio.Flow.copy_string json_data flow
;;

(* Handle SSE events endpoint *)
let handle_events_endpoint flow clock =
  let client_id = generate_alphabetic_id !client_counter in
  incr client_counter;
  log_message (Printf.sprintf "Client %s connecting..." client_id);
  let connected_at = Unix.time () *. 1000.0 in
  let client =
    { id = client_id; connected_at; active = true; last_ping = Unix.time () }
  in
  (* Add client to hashtable *)
  Mutex.lock clients_mutex;
  Hashtbl.add clients client_id client;
  let total_clients = Hashtbl.length clients in
  Mutex.unlock clients_mutex;
  log_message
    (Printf.sprintf "Client %s connected. Total clients: %d" client_id total_clients);
  (* Send SSE headers *)
  send_http_headers flow "text/event-stream" [];
  (* Send initial connected message *)
  send_sse_event
    flow
    "connected"
    client_id
    "OCaml SSE connection established"
    connected_at
    total_clients;
  (* Start continuous ping loop - direct socket writes will detect disconnections immediately *)
  let rec ping_loop () =
    if not client.active
    then cleanup_client client_id
    else (
      Eio.Time.sleep clock 0.5;
      if client.active
      then (
        let timestamp = Unix.time () *. 1000.0 in
        Mutex.lock clients_mutex;
        let total_clients = Hashtbl.length clients in
        Mutex.unlock clients_mutex;
        try
          (* Send heartbeat comment *)
          Eio.Flow.copy_string ": heartbeat\n" flow;
          (* Send ping event - this will throw exception immediately on disconnection *)
          send_sse_event flow "ping" client_id "ping from OCaml" timestamp total_clients;
          client.last_ping <- Unix.time ();
          log_message (Printf.sprintf "Ping sent to %s" client_id);
          ping_loop ()
        with
        | exn ->
          log_message
            (Printf.sprintf
               "Client %s disconnected: %s"
               client_id
               (Printexc.to_string exn));
          cleanup_client client_id)
      else cleanup_client client_id)
  in
  ping_loop ()
;;

(* Handle status endpoint *)
let handle_status_endpoint flow =
  Mutex.lock clients_mutex;
  let clients_list = Hashtbl.fold (fun _ client acc -> client :: acc) clients [] in
  let total_clients = List.length clients_list in
  Mutex.unlock clients_mutex;
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
  "implementation": "OCaml/Eio (immediate disconnection detection)"
}|}
      total_clients
      clients_json
  in
  (* Send JSON response *)
  send_http_headers flow "application/json" [];
  Eio.Flow.copy_string status_json flow
;;

(* Handle OPTIONS endpoint *)
let handle_options_endpoint flow =
  let additional_headers =
    [ "Access-Control-Allow-Methods", "GET, OPTIONS"
    ; "Access-Control-Allow-Headers", "Content-Type"
    ]
  in
  send_http_headers flow "text/plain" additional_headers
;;

(* Handle 404 *)
let handle_404 flow =
  let response =
    "HTTP/1.1 404 Not Found\r\n"
    ^ "Content-Type: text/plain\r\n"
    ^ "Access-Control-Allow-Origin: *\r\n"
    ^ "\r\n"
    ^ "Not Found\n\nAvailable endpoints:\n- /events (SSE)\n- /status (JSON)"
  in
  Eio.Flow.copy_string response flow
;;

(* Handle client connection *)
let handle_client clock flow _addr =
  try
    match parse_http_request flow with
    | Some ("GET", "/events") -> handle_events_endpoint flow clock
    | Some ("GET", "/status") -> handle_status_endpoint flow
    | Some ("OPTIONS", _) -> handle_options_endpoint flow
    | _ -> handle_404 flow
  with
  | exn -> log_message (Printf.sprintf "Connection error: %s" (Printexc.to_string exn))
;;

(* Background cleanup task *)
let cleanup_stale_clients clock =
  let rec cleanup_loop () =
    Eio.Time.sleep clock 2.0;
    let now = Unix.time () in
    Mutex.lock clients_mutex;
    let stale_clients =
      Hashtbl.fold
        (fun id client acc ->
           if now -. client.last_ping > 3.0
           then (* 3 seconds without ping = stale *)
             id :: acc
           else acc)
        clients
        []
    in
    Mutex.unlock clients_mutex;
    List.iter
      (fun client_id ->
         log_message (Printf.sprintf "Detected stale client %s - cleaning up" client_id);
         cleanup_client client_id)
      stale_clients;
    cleanup_loop ()
  in
  cleanup_loop ()
;;

(* Main server *)
let run_server env =
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  Printf.printf "\n";
  Printf.printf "🟤 OCaml Eio SSE Server Starting (Port %d)\n" port;
  Printf.printf "✅ This implementation provides immediate disconnection detection:\n";
  Printf.printf "   - Eio effects-based I/O\n";
  Printf.printf "   - Direct socket writes without buffering\n";
  Printf.printf "   - Structured concurrency\n";
  Printf.printf "   - Immediate exception on client disconnect\n";
  Printf.printf "\n";
  Printf.printf "Expected behavior: IMMEDIATE disconnection detection\n";
  Printf.printf "\n";
  Printf.printf "Endpoints:\n";
  Printf.printf "  - http://localhost:%d/events (SSE stream)\n" port;
  Printf.printf "  - http://localhost:%d/status (Active connections)\n" port;
  Printf.printf "\n";
  flush_all ();
  Eio.Switch.run ~name:"main"
  @@ fun sw ->
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:10 addr in
  log_message "Eio server started successfully";
  (* Get clock from environment *)
  let clock = Eio.Stdenv.clock env in
  (* Start background cleanup task *)
  Eio.Fiber.fork_daemon ~sw (fun () -> cleanup_stale_clients clock);
  (* Run the server *)
  Eio.Net.run_server server (handle_client clock) ~on_error:(fun exn ->
    log_message (Printf.sprintf "Server error: %s" (Printexc.to_string exn)))
;;

let () =
  (* Set stdout to be unbuffered for real-time logging *)
  set_binary_mode_out stdout false;
  (* Setup signal handlers for graceful shutdown *)
  Sys.set_signal
    Sys.sigterm
    (Sys.Signal_handle
       (fun _ ->
         Printf.printf "\nReceived SIGTERM, shutting down...\n";
         flush_all ();
         exit 0));
  Sys.set_signal
    Sys.sigint
    (Sys.Signal_handle
       (fun _ ->
         Printf.printf "\nReceived SIGINT, shutting down...\n";
         flush_all ();
         exit 0));
  (* Start the server *)
  Eio_main.run run_server
;;

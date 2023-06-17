(* === JSON === *)

type event =
  { code  : string
  ; label : string
  ; date  : string
  }
[@@deriving of_yojson]
[@@ocamlformat "disable"]

type shipment =
  { events : event list [@key "event"]
  }
[@@deriving of_yojson { strict = false }]
[@@ocamlformat "disable"]

type payload =
  { status   : int             [@key "returnCode"]
  ; shipment : shipment option [@default None]
  }
[@@deriving of_yojson { strict = false}]
[@@ocamlformat "disable"]

(* == PARSING == *)

let parse_json str =
  let ( let* ) = Result.bind in
  let root = Yojson.Safe.from_string str in
  let pretty = Yojson.Safe.pretty_to_string root in
  let* payload = payload_of_yojson root in
  let delivered =
    Option.bind payload.shipment (fun shipment ->
        shipment.events
        |> List.fold_left
             (fun acc x ->
               if x.code = Domain.delivered_code then
                 match Ptime.of_rfc3339 ~strict:true x.date with
                 | Error _ -> None
                 | Ok (t, offset, _) ->
                     Some
                       ({ on = (t, offset); label = x.label }
                         : Domain.delivered)
               else
                 acc)
             None)
  in

  let shipping_status = payload.status in
  let delivery_status : Domain.delivery_status =
    { shipping_status; delivered }
  in
  Ok (delivery_status, pretty)
;;

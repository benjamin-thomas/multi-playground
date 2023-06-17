let string_of_delivered opt =
  let to_str t offset =
    (* a bit annoying having to do that!*)
    match offset with
    | None -> Ptime.to_rfc3339 t
    | Some tz_offset_s -> Ptime.to_rfc3339 t ~tz_offset_s
  in
  opt
  |> Option.map (fun (x : Domain.delivered) ->
         let on, offset = x.on in
         Printf.sprintf "Delivered on: %s (%s)" (to_str on offset) x.label)
  |> Option.value ~default:"NOT YET"
;;

let text_summary status_code tracking_number (delivery_status : Domain.delivery_status) =
  Dedent.string @@
    Printf.sprintf {|
      Tracking number         : %s
      API HTTP status code    : %d
      Shipping status code    : %d
      Current delivery status : %s
      |}
    tracking_number
    status_code
    delivery_status.shipping_status
    (string_of_delivered delivery_status.delivered)
[@@ocamlformat "disable"]

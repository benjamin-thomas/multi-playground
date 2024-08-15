(* Cmd GADT *)
type _ cmd =
  | Invite : string -> unit cmd
  | Here : string list cmd
  | Status : string -> unit cmd
  | Bind : 'a cmd * ('a -> 'b cmd) -> 'b cmd

module Cmd = struct
  let bind x f = Bind (x, f)
end

type app_state =
  { started_at : float
  ; users : string list
  ; counter : int
  }

let pp_app_state fmt state =
  let started_at_str = Printf.sprintf "%.0f" state.started_at in
  let users_str = String.concat ", " state.users in
  Format.fprintf
    fmt
    "@[<v>App State:@, Started at: %s@, Users: [%s]@, Counter: %d@]"
    started_at_str
    users_str
    state.counter
;;

let rec interpret : type a. a cmd -> (app_state, a) State.t = function
  | Bind (f, g) -> State.bind (interpret f) (fun x -> interpret (g x))
  | Invite user_name ->
    State.modify (fun state ->
      let new_state =
        { state with users = user_name :: state.users; counter = state.counter + 1 }
      in
      print_endline ("You invited @" ^ user_name ^ "!");
      new_state)
  | Here ->
    [%do
      begin
        state <- State.get;
        let users = state.users in
        let () =
          print_endline
            ("In this channel: "
             ^ String.concat " " users
             ^ " (counter="
             ^ string_of_int state.counter
             ^ ")")
        in
        State.return users
      end [@monad State]]
  | Status descr ->
    [%do
      begin
        state <- State.get;
        let counter = state.counter + 10 in
        let () =
          print_endline
            ("Setting status to '"
             ^ descr
             ^ "' (old_counter="
             ^ string_of_int state.counter
             ^ ", new_counter="
             ^ string_of_int counter
             ^ ")")
        in
        State.modify (fun state -> { state with counter });
        State.return ()
      end [@monad State]]
;;

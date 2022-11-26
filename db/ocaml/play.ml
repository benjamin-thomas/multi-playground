(* [@@@warning "A-40-42"] All errors and disable warnings 40+42 *)
(* [@@@warning "A"] *)
module Config = struct
  type display_mode =
    | Light
    | Dark

  type config =
    | Setting of
        { font_size : int
        ; mode : display_mode
        }

  let default : config = Setting { font_size = 16; mode = Dark }
  let withLight (Setting config) = Setting { config with mode = Light }
end

let user_config = Config.default
let user_config_light = Config.default |> Config.withLight
let greet_alt ~fname ~lname = "Hello " ^ fname ^ " " ^ lname ^ "!"

module User = struct
  type first_name = FirstName of string
  type last_name = LastName of string

  let with_first_name str = FirstName str
  let with_last_name str = LastName str
  let greet (FirstName fn) (LastName ln) = "Hello " ^ fn ^ " " ^ ln ^ "!"
end

module AdminUser : sig
  type first_name
  type last_name

  val first_name_from_string : string -> first_name
  val last_name_from_string : string -> last_name
  val greet : first_name -> last_name -> string
end = struct
  type first_name = FirstName of string
  type last_name = LastName of string

  let first_name_from_string str = FirstName str
  let last_name_from_string str = LastName str
  let greet (FirstName fn) (LastName ln) = "Granted admin access: " ^ fn ^ " " ^ ln ^ "!"
end

module SuperUser : sig
  type t
  type first_name = FirstName of string
  type last_name = LastName of string

  val init : first_name -> last_name -> t
  val greet : t -> string
end = struct
  type first_name = FirstName of string
  type last_name = LastName of string
  type t = Name of first_name * last_name

  let init fn ln = Name (fn, ln)

  (* let greet (FirstName fn) (LastName ln) = "Granted admin access: " ^ fn ^ " " ^ ln ^ "!" *)
  let greet (Name (FirstName fn, LastName ln)) =
    "Granted super access: " ^ fn ^ " " ^ ln ^ "!"
  ;;
end

let print_bob () =
  print_endline @@ User.greet (User.FirstName "Bob") (User.LastName "Doe")
  [@@warning "A"]
;;

let print_ben_super () =
  (* let open SuperUser in *)
  let su = SuperUser.init (FirstName "Benjamin") (LastName "Thomas") in
  print_endline @@ SuperUser.greet su
  [@@warning "A-40-42"]
;;

let print_sam () =
  let open User in
  print_endline @@ User.greet (FirstName "Sam") (LastName "Doe")
  [@@warning "A"]
;;

let print_elton () = print_endline @@ User.(greet (FirstName "Elton") (LastName "Doe"))
  [@@warning "A"]
;;

(* Below won't compile: good! *)
(* let print_admin () = print_endline @@ AdminUser.greet (FirstName "Bob") (LastName "Doe") *)
(* Must go through fn/ln constructors *)
let print_admin () =
  print_endline
  @@ AdminUser.greet
       (AdminUser.first_name_from_string "Bob")
       (AdminUser.last_name_from_string "Doe")
  [@@warning "A"]
;;

(* Short version of above *)
let print_admin' () =
  let open AdminUser in
  print_endline @@ greet (first_name_from_string "Bob") (last_name_from_string "Doe")
  [@@warning "A"]
;;

let _ =
  let john = User.FirstName "John" in
  print_endline @@ User.greet john (LastName "Doe");
  let jane = User.with_first_name "Jane" in
  let doe = User.with_last_name "Doe" in
  print_endline @@ User.greet jane doe;
  print_bob ();
  print_sam ();
  print_elton ();
  print_admin ();
  print_admin' ();
  print_ben_super ();
  print_endline @@ greet_alt ~fname:"John" ~lname:"Doe"
;;

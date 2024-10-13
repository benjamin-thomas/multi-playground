(*
   dune exec -w --no-print-directory --display=quiet ./many_counters.exe
*)

open Printf

module Counter1 : sig
  type t

  val create : unit -> t
  val inc : t -> unit
  val get_val : t -> int
end = struct
  type t =
    { mutable n : int
    ; lock : Mutex.t
    }

  let create () = { n = 0; lock = Mutex.create () }

  let inc c =
    Mutex.lock c.lock;
    c.n <- c.n + 1;
    Mutex.unlock c.lock;
    ()
  ;;

  let get_val c = c.n
end

module Counter2 : sig
  type t

  val create : unit -> t
  val inc : t -> unit
  val get_val : t -> int
end = struct
  type t = { n : int Atomic.t }

  let create () = { n = Atomic.make 0 }
  let inc t = Atomic.incr t.n
  let get_val t = Atomic.get t.n
end

module Counter3 : sig
  type t

  val create : unit -> t
  val inc : t -> unit
  val get_val : t -> int
end = struct
  type t = int Atomic.t

  let create () = Atomic.make 0
  let inc t = Atomic.incr t
  let get_val t = Atomic.get t
end

let () =
  let c1 = Counter1.create () in
  let c2 = Counter2.create () in
  let c3 = Counter3.create () in
  let domains = ref [] in
  for _ = 1 to 3 do
    let d =
      Domain.spawn (fun () ->
        for _ = 1 to 100000 do
          Counter1.inc c1;
          Counter2.inc c2;
          Counter3.inc c3;
          ()
        done)
    in
    domains := d :: !domains;
    ()
  done;
  List.iter Domain.join !domains;
  printf "(OCaml) 3 domains changed Counter1's value to: %d\n" (Counter1.get_val c1);
  printf "(OCaml) 3 domains changed Counter2's value to: %d\n" (Counter2.get_val c2);
  printf "(OCaml) 3 domains changed Counter3's value to: %d\n" (Counter3.get_val c3);
  ()
;;

(*
   dune exec -w --no-print-directory --display=quiet ./many_counters_eio.exe
*)

open Eio.Std

let inc_with_eio mutex counter =
  for _ = 1 to 10000 do
    Eio.Mutex.use_rw ~protect:true mutex (fun () -> counter := !counter + 1)
  done
;;

let counter2 = Atomic.make 0

let inc_without_eio () =
  for _ = 1 to 10000 do
    Atomic.incr counter2
  done
;;

let main _env =
  let counter1 = ref 0 in
  let mutex = Eio.Mutex.create () in
  Eio.Fiber.all
    [ (fun () -> inc_with_eio mutex counter1)
    ; (fun () -> inc_with_eio mutex counter1)
    ; (fun () -> inc_with_eio mutex counter1)
    ; (fun () -> inc_without_eio ())
    ; (fun () -> inc_without_eio ())
    ; (fun () -> inc_without_eio ())
    ];
  traceln "Final counter1 value: %d" !counter1;
  traceln "Final counter2 value: %d" @@ Atomic.get counter2;
  ()
;;

let () = Eio_main.run main

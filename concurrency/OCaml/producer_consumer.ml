open Printf

let mutex = Mutex.create ()
let condition = Condition.create ()

module Producer = struct
  let run queue consumers_cnt =
    for _ = 1 to 7777 do
      Mutex.lock mutex;
      Queue.push 1 queue;
      Condition.signal condition;
      Mutex.unlock mutex
    done;
    Mutex.lock mutex;
    for _ = 1 to consumers_cnt do
      (* Send a "poison pill" to end the consumers *)
      Queue.push (-1) queue
    done;
    Condition.broadcast condition;
    Mutex.unlock mutex
  ;;
end

module Consumer : sig
  type t = int ref

  val create : unit -> t
  val run : int ref -> int Queue.t -> unit
end = struct
  type t = int ref

  let create () = ref 0

  let run total queue =
    let rec consume () =
      Mutex.lock mutex;
      while Queue.is_empty queue do
        Condition.wait condition mutex
      done;
      let n = Queue.pop queue in
      Mutex.unlock mutex;
      if n != -1
      then (
        total := !total + n;
        consume ())
    in
    consume ()
  ;;
end

let () =
  let consumers = List.init 3 (fun _ -> Consumer.create ()) in
  let all_domains =
    let queue = Queue.create () in
    let x = Domain.spawn (fun () -> Producer.run queue (List.length consumers)) in
    let xs =
      List.map (fun c -> Domain.spawn (fun () -> Consumer.run c queue)) consumers
    in
    x :: xs
  in
  List.iter Domain.join all_domains;
  printf "\n\n==== Result ====\n";
  List.iter (fun c -> printf "Sub-total:  %d\n" !c) consumers;
  print_endline "----------------";
  printf "Total: %9d\n" (List.fold_left (fun acc c -> acc + !c) 0 consumers);
  ()
;;

(*
    echo monads.fsx | entr -c dotnet fsi /_
*)
let (>>=) o f =
    match o with
    | Some x -> f x
    | None -> None

let add x y =
    x >>= fun x ->
    y >>= fun y ->
    Some (x + y)

let mult x y =
    x |> Option.bind (fun x -> 
    y |> Option.bind (fun y -> 
    Some(x * y)))

let printRes res =
    match res with
    | None -> printfn "None"
    | Some a -> printfn "Some %O" a


printf "\n--- MONAD EXAMPLE ---\n\n"
printf "add  (Some 1) (Some 2)     =>   ";; printRes <| add  (Some 1) (Some 2);;
printf "add  (Some 1)  None        =>   ";; printRes <| add  (Some 1)  None;;
printf "add   None    (Some 2)     =>   ";; printRes <| add   None    (Some 2);;
printf "add   None     None        =>   ";; printRes <| add   None     None;;
printf "---\n";;
printf "mult (Some 2) (Some 3)     =>   ";; printRes <| mult (Some 2) (Some 3);;
printf "mult  None    (Some 3)     =>   ";; printRes <| mult  None    (Some 3);;
printf "mult (Some 2)  None        =>   ";; printRes <| mult (Some 2)  None;;
printf "mult  None     None        =>   ";; printRes <| mult  None     None;;

let (>>=) o f =
    match o with
    | Some x -> f x
    | None -> None

let add x y =
    x >>= fun x ->
    y >>= fun y ->
    Some (x + y)

let printRes res =
    match res with
    | None -> printfn "None"
    | Some a -> printfn "Some %O" a


// dotnet fsi monads.fsx
printf "\n--- MONADS EXAMPLE ---\n\n"
printf "add (Some 1) (Some 2)      =>   ";; printRes <| add (Some 1) (Some 2);;
printf "add (Some 1) None          =>   ";; printRes <| add (Some 1) None;;
printf "add None     (Some 2)      =>   ";; printRes <| add None (Some 2);;
printf "add None     None          =>   ";; printRes <| add None None;;

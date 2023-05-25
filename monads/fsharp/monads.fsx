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

// Pipeline builder, Elm style
let add' a b c =
    Ok(fun x y z -> x + y + z)
    |> Result.bind (fun fn -> Result.map fn a)
    |> Result.bind (fun fn -> Result.map fn b)
    |> Result.bind (fun fn -> Result.map fn c)

// https://fsharpforfunandprofit.com/posts/computation-expressions-intro/
type MaybeBuilder() =
    member this.Bind(x, f) =
        match x with
        | None -> None
        | Some a -> f a
    member this.Return(x) =
        Some x

let maybe = new MaybeBuilder()

let minus x y =
    maybe {
        let! x = x
        let! y = y
        return x - y
    }

let printRes res =
    match res with
    | None -> printfn "None"
    | Some a -> printfn "Some %O" a

let printRes2 res =
    match res with
    | Error x -> printfn "Error %O" x
    | Ok x -> printfn "Ok %O" x

printf "\n--- MONAD EXAMPLE ---\n\n"
printf "add    (Some 1) (Some 2)     =>   ";; printRes <| add (Some 1) (Some 2);;
printf "add    (Some 1)  None        =>   ";; printRes <| add (Some 1) None;;
printf "add     None    (Some 2)     =>   ";; printRes <| add None (Some 2);;
printf "add     None     None        =>   ";; printRes <| add None None;;
printf "---\n";;
printf "add' (Ok 1) (Ok 2) (Ok 3)           =>   ";; printRes2 <| add' (Ok 1) (Ok 2) (Ok 3);;
printf "add' (Ok 1) (Error \"2\") (Error \"3\") =>   ";; printRes2 <| add' (Ok 1) (Error "2") (Error "3");;
printf "---\n";;
printf "minus  (Some 2) (Some 1)     =>   ";; printRes <| minus (Some 2) (Some 1);;
printf "minus  (Some 2)  None        =>   ";; printRes <| minus (Some 2) None;;
printf "minus   None    (Some 1)     =>   ";; printRes <| minus None (Some 1);;
printf "minus   None     None        =>   ";; printRes <| minus None None;;
printf "---\n";;
printf "mult   (Some 2) (Some 3)     =>   ";; printRes <| mult (Some 2) (Some 3);;
printf "mult    None    (Some 3)     =>   ";; printRes <| mult None (Some 3);;
printf "mult   (Some 2)  None        =>   ";; printRes <| mult (Some 2) None;;
printf "mult    None     None        =>   ";; printRes <| mult None None;;

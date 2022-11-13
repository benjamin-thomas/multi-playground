(*
    Write a simple script that, given a folder path on the fs:
    
        - will return the name and size of each subfolder
        
    TODO/FIXME:
        I only printed a tree structure here.
        I still have to print the size of each folders
        Also, empty dirs are not shown (see comment in Tree.fs)
*)

[<EntryPoint>]
let main args =
    if args.Length = 0 then
        printfn "Must provide a path!"
    else
        let path = args[0]
        Tree.print path
    0

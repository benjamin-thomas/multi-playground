(*
  rg --files | entr -c dune exec tree
  while true;do rg --files | entr -c dune exec tree;sleep 1;done
  clear && dune build && _build/default/bin/main.exe ~/code/explore/elm/turtle/
  rg --files | entr -c bash -c 'dune build && _build/default/bin/main.exe ~/code/explore/elm/turtle/'
  dune exec tree ~/code/explore/elm/turtle/
*)

(*
    Copy/paste into utop:

        > #use_output "dune ocaml top";;
        > Lib__Tree.enum_entries "/home/benjamin/code/explore/elm/turtle/";;
        > Lib.Tree.enum_entries "/home/benjamin/code/explore/elm/turtle/";;

    Or use `dune utop ./lib/`
    Or use `dune utop`
    while true;do clear && dune utop ./lib/;sleep 200ms;done

    Alternative workflows

    $ ocaml
    > #use "down.top"
    Then use Ctr-T to show the docs
*)

let () = Lib.Tree.print Sys.argv.(1)

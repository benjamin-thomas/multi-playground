// dune exec --no-print-directory --display=quiet ./main_jsx.exe -w

/*

 opam install reason (provides refmt)

  */

open Tyxml;

// Equivalent to "let in" syntax
let _double_plus_one = x => {
  let x2 = x * 2;
  x2 + 1;
};

type user = {
  name: string,
  age: int,
};

let users = [
  {name: "John", age: 30},
  {name: "Jane", age: 25},
  {name: "Bob", age: 40},
];

let user_li = (u: user) =>
  <li> {Html.txt(u.name ++ " is " ++ string_of_int(u.age))} </li>;

let mytitle = Html.txt("A Fabulous Web Page");

let mycontent = (n: int) => {
  let lis = users |> List.map(user_li);
  <div className="content">
    <h1> {Html.txt("My title #" ++ string_of_int(n))} </h1>
    <h2> "This is a fabulous content." </h2>
    <ul> ...lis </ul>
  </div>;
};

let mypage =
  <html>
    <head> <title> mytitle </title> </head>
    <body> {mycontent(1)} </body>
  </html>;

let () = {
  let fmt = Format.formatter_of_out_channel(stdout);
  Format.fprintf(fmt, "%a@.", Html.pp(~indent=true, ()), mypage);
};

import java.util.Scanner;
import java.io.File;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.System;

eff App {
    pub def readln(): Option[String]
    pub def println(s: String): Unit
}

def runApp(f: Unit -> t \ App): t \ IO = {
    let scanner = new Scanner(System.in);
    try f() with App {
        def readln(k) =
            k(
                if (scanner.hasNextLine())
                    Some(scanner.nextLine())
                else
                    None
            )

        def println(s, k) = {
            println(s);
            k()
        }
    }
}

@test
def toBase10_test(): Bool = {
    Assert.eq(toBase10('0'), Some(0))
    and
    Assert.eq(toBase10('9'), Some(9))
    and
    Assert.eq(toBase10('a'), Some(10))
    and
    Assert.eq(toBase10('A'), Some(10))
    and
    Assert.eq(toBase10('B'), Some(11))
    and
    Assert.eq(toBase10('F'), Some(15))
    and
    Assert.eq(toBase10('z'), Some(35))
    and
    Assert.eq(toBase10('?'), None)
}

def toBase10(c: Char): Option[Int32] = {
    if (Char.isDigit(c)) Char.digit({radix=10}, c)
    else if (Char.isUpperCase(c)) Some(Char.toBmpCodePoint(c) - Char.toBmpCodePoint('A') + 10)
    else if (Char.isLowerCase(c)) Some(Char.toBmpCodePoint(c) - Char.toBmpCodePoint('a') + 10)
    else None
}

@test
def convertToBase10_test(): Bool =
    Assert.eq(
        (12 :: 11 :: 10 :: Nil, Nil),
        convertToBase10(16, 'a' :: 'b' :: 'c' :: Nil)
    ) and
    Assert.eq(
        (12 :: 11 :: 10 :: Nil, Nil),
        convertToBase10(16, ('A' :: 'B' :: 'C' :: Nil))
    ) and
    Assert.eq(
        ((35 :: 9 :: Nil), ('!' :: Nil)),
        convertToBase10(36, ('9' :: '!' :: 'Z' :: Nil))
    ) and
    Assert.eq(
        (Nil, 'Z' :: '!' :: '8' :: Nil),
        convertToBase10(37, '8' :: '!' :: 'Z' :: Nil)
    )

def convertToBase10(base: Int32, chars: List[Char]): (List[Int32], List[Char]) = {
   List.foldLeft
    ( (acc, c) ->
        let (good, bad) = acc;
        match toBase10(c) {
            case None =>
                (good, c::bad)

            case Some(n) => {
                if (n < 0 or n >= base or base > 36 or base < 2)
                    (  good, c::bad)
                else
                    (n::good,   bad)
            }
        }
    , (Nil, Nil)
    , chars
    )
}


def horner(base: Int32, vals: List[Int32]): Int32 =
    List.foldRight( (v, acc) -> acc * base + v
                  , 0
                  , vals
                  )

def handleLine(base: Int32, line: String): Unit \ App = {
    let (good, bad) = convertToBase10(base, String.toList(line));
    match (good, bad) {
        case (vals, Nil) => {
            do App.println("${List.reverse(vals)}"); // for clarity, for the user
            do App.println("  -> ${horner(base, vals)}")
        }
        case (_, chars) => {
            do App.println("  => Bad chars: " + ToString.toString(chars))
        }
    }
}

def appProgram(base: Int32): Unit \ App = {
    let mLine = do App.readln();
    match mLine {
        case None => {
            do App.println("Finished(exit appProgram)")
        }
        case Some(line) => {
            do App.println(line);
            handleLine(base, line);
            do App.println("");
            appProgram(base)
        }
    }
}


/*

flix run --args 16 < <(echo -e "ABC\n123\nFF\n1X")
flix test

---

flix repl
flix> :test

flix> mainLoop(16)

 */
def mainLoop(base: Int32): Unit \ IO =
    println("Base is: ${base}\n");
    runApp(() -> appProgram(base));
    println("Finished(exit mainLoop)")


/*

The standard library documentation is available at:

https://api.flix.dev/


Do this to automatically run the tests on file change:

- terminal 1
  - flix repl
- terminal 2
  - echo ./Main.flix | entr tmux send-keys -t ":1" C-l \; send-keys -t ":1" ":test" Enter

*/
def main(): Unit \ IO =
    match Environment.getArgs() {
        case baseStr ::_ =>
            match Int32.fromString(baseStr) {
                case None => println("Invalid base")
                case Some(base) => mainLoop(base)
            }
        case Nil =>
            println("Usage: Main.flix BASE")
    };
    println("Finished(exit main)")



// def main2(): Unit \ IO =
//    let result = forM (
//        baseStr <- Environment.getArgs()
//                     |> List.head
//                     |> Option.toOk("Missing argument: BASE");

//        base <- Int32.fromString(baseStr)
//                  |> Option.toOk("Invalid base argument")
//    ) yield mainLoop(base);

//    match result {
//        case Err(err) => println(err)
//        case Ok(())   => println("Finished!")
//    }


// def main3(): Unit \ IO =
//    let result =
//        Environment.getArgs()
//            |> List.head
//            |> Option.toOk("Missing argument: BASE")
//            |> Result.flatMap(
//                   Int32.fromString >> Option.toOk("Invalid base argument"))
//            |> Result.map(mainLoop);

//    match result {
//        case Err(err) => println(err)
//        case Ok(())   => println("Finished!")
//    }

// def main4(): Unit \ IO =
//    let x = forM (
//        baseStr <- Environment.getArgs()
//                     |> List.head;

//        base <- Int32.fromString(baseStr)
//    ) yield mainLoop(base);

//    match x {
//        case None   => println("Usage: Main.flix BASE")
//        case Some() => println("Finished!")
//    }


// def divide(x: Int32, y: Int32): Option[Int32] =
//     if (y == 0) None else Some(x / y)

// def f(): Option[Int32] =
//     forM (
//         x <- divide(5, 2);
//         y <- divide(x, 8);
//         z <- divide(9, y)
//     ) yield x + y + z

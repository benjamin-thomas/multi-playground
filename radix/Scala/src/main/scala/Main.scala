import scala.annotation.tailrec

val USAGE = "Usage: ./Main BASE [STDIN]"

def die(str: String): Unit =
  println(str)
  sys.exit(1)

def charToDigit(c: Char): Option[Int] =
  if c >= '0' && c <= '9' then
    Some(c.toInt - Char.char2int('0'))
  else if c >= 'A' && c <= 'Z' then
    Some(c.toInt - Char.char2int('A') + 10)
  else if c >= 'a' && c <= 'z' then
    Some(c.toInt - Char.char2int('a') + 10)
  else
    None

def convertToBase10(base: Int, chars: List[Char]): (List[Int], List[Char]) =
  chars.foldRight((List.empty[Int], List.empty[Char])) { (c, acc) =>
    val (good, bad) = acc
    charToDigit(c) match {
      case None => (good, c :: bad)
      case Some(n) =>
        if n < 0 || n >= base || base < 2 || base > 36 then
          (good, c :: bad)
        else
          (n :: good, bad)
    }
  }

def horner(base: Int, lst: List[Int]): Int =
  lst.foldLeft(0) { (acc, n) =>
    n + (acc * base)
  }

def handleLine(base: Int, line: String): Unit =
  def printGood(chars: List[Int], value: Int): Unit =
    println(chars.mkString(" "))
    println(s"-> $value\n\n")
  
  def printBad(chars: List[Char]): Unit =
    println(s"Bad chars: ${chars.mkString("")}")
    
  convertToBase10(base, line.toList) match {
    case (good, Nil) =>
      printGood(good, horner(base, good))
    case (_, bad) =>
      printBad(bad)
  }

@tailrec
def mainLoop(base: Int): Unit =
  scala.io.StdIn.readLine() match
    case null => ()
    case line =>
      handleLine(base, line)
      mainLoop(base)

def startProgram(baseStr: String): Unit =
  baseStr.toIntOption match {
    case None => die(s"Not a numeric value: $baseStr")
    case Some(base) =>
      println(s"Base is $base\n\n")
      mainLoop(base)
      println("Done!")
  }

/*

Run with:
  sbt "run 16"

Start the REPL with:
  sbt ~console
The ctrl-D to re-enter to play with the newly compiled code

Monitor the compilation output or run the program with with:
  sbt
  sbt:radix> ~clear;compile
  sbt:radix> ~clear;run 16


*/
@main def main(args: String*): Unit =
  args match {
    case base :: Nil =>
      startProgram(base)
    case _ =>
      println(USAGE)
  }


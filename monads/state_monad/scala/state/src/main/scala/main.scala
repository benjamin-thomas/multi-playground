import cats.data.State

//def random(seed: Long): Long = seed * 6364136223846793005L + 1442695040888963407L

def random(seed: Long): Long = {
  val (a, c, m) = (1103515245, 12345, 2147483648L)
  (a * seed + c) % m
}

type Random[A] = State[Long, A]

def randNumVerbose: Random[Long] =
  State.get.flatMap { seed =>
    val newSeed = random(seed)
    State.set(newSeed).flatMap { (_: Unit) =>
      State.pure(newSeed)
    }
  }

val randTup3Verbose: Random[(Long, Long, Long)] = {
  randNumVerbose.flatMap { a =>
    randNumVerbose.flatMap { b =>
      randNumVerbose.flatMap { c =>
        State.pure(a, b, c)
      }
    }
  }
}

def randNumForComprehension: Random[Long] =
  for {
    seed: Long <- State.get
    newSeed = random(seed)
    _: Unit <- State.set(newSeed)
  } yield newSeed


val randTup3ForComprehension: Random[(Long, Long, Long)] = {
  for {
    a <- randNumForComprehension
    b <- randNumForComprehension
    c <- randNumForComprehension
  } yield (a, b, c)
}

@main
def main(): Unit = {
  val (a, b, c) = randTup3Verbose.runA(0).value
  println(s"Got tup3 (verbose): ($a, $b, $c)")

  val (a2, b2, c2) = randTup3ForComprehension.runA(0).value
  println(s"Got tup3 (for comprehension): ($a2, $b2, $c2)")
}

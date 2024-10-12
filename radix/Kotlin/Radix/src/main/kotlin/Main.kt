package org.example

import kotlin.system.exitProcess

const val USAGE = "Usage: ./Main BASE [STDIN]"

fun main(args: Array<String>) {
    start(args)
}

fun <T> requireArg(args: Array<String>, idx: Int, msg: String, conv: (String) -> T): T {
    try {
        return conv(args[idx])
    } catch (_: RuntimeException) {
        println(msg)
        exitProcess(1)
    }
}

fun horner(base: Int, nums: List<Int>): Int =
    nums.fold(0) { acc, n -> n + (acc * base) }


fun charToDigit(c: Char): Result<Int> {
    return if (c.isDigit()) {
        Result.success(c.code - '0'.code)
    } else if (c in 'A'..'F') {
        Result.success(c.code - 'A'.code + 10)
    } else if (c in 'a'..'f') {
        Result.success(c.code - 'a'.code + 10)
    } else {
        Result.failure(IllegalArgumentException("Invalid char: $c"))
    }
}

fun convertToBase10(base: Int, chars: List<Char>): Pair<List<Int>, List<Char>> {
    val good = mutableListOf<Int>()
    val bad = mutableListOf<Char>()
    for (c in chars) {
        charToDigit(c)
            .onFailure { bad.add(c) }
            .onSuccess {
                if (it < 0 || it >= base || base < 2 || base > 36) {
                    bad.add(c)
                } else {
                    good.add(it)
                }
            }
    }
    return Pair(good, bad)
}

fun handleLine(base: Int, line: String) {
    fun printGood(chars: List<Int>, value: Int) {
        println(chars.joinToString(" "))
        println("-> $value")
    }

    fun printBad(chars: List<Char>) {
        println("Bad chars: $chars")
    }

    val (good, bad) = convertToBase10(base, line.toList())
    if (bad.isNotEmpty()) {
        printBad(bad)
    } else {
        printGood(good, horner(base, good))
    }
}

fun mainLoop(base: Int) {
    while (true) {
        val line = readLine() ?: break
        handleLine(base, line)
    }
}

fun start(args: Array<String>) {
    val base = requireArg(args, 0, USAGE) { it.toInt() }
    println("Base is $base")
    mainLoop(base)
    println("Done!")
}

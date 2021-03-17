package oscar.cp.examples


import oscar.cp._

/**
 * Find a number consisting of 9 digits in which each of the digits from
 * 1 to 9 appears only once. This number must also satisfy these divisibility requirements:
 * 1. The number should be divisible by 9.
 * 2. If the rightmost digit is removed, the remaining number should be divisible by 8.
 * 3. If the rightmost digit of the new number is removed, the remaining number should be divisible by 7.
 * 4. And so on, until there's only one digit (which will necessarily be divisible by 1).
 *
 *
 */
object DivisibleBy9Through1 extends CPModel with App {

  val digits = Array.fill(9)(CPIntVar(1 to 9))
  val numbers = Array.fill(9)(CPIntVar(1 to 1000000000))
  val divisors = Array.fill(9)(CPIntVar(1 to 100000000))

  val coefs = Array(100000000, 10000000, 1000000, 100000, 10000, 1000, 100, 10, 1)

  onSolution {
    println(digits.mkString(", "))
    println(numbers.mkString(", "))
  }

  add(allDifferent(digits), Strong)
  for (i <- 1 to 9) {
    add(sum(0 until i)(j => digits(j) * coefs.drop(9 - i)(j)) === numbers(i - 1))
    add(numbers(i - 1) === divisors(i - 1) * i)
  }

  search(binaryFirstFail(digits))

  val stats = start()

  println(stats)
}

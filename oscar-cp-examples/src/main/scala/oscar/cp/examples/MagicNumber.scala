package oscar.cp.examples

import oscar.cp._

/**
 * Create a number using only the digits 4,4,3,3,2,2,1 and 1.
 * So it can only be eight digits.
 *
 * You have to make sure that:
 * - the ones are separated by one digit;
 * - the twos are separated by two digits;
 * - the threes are separated with three digits;
 * - the fours are separated by four digits.
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object MagicNumber extends CPModel with App {

  val one = CPIntVar(0 to 7)
  val two = CPIntVar(0 to 7)
  val three = CPIntVar(0 to 7)
  val four = CPIntVar(0 to 7)
  val x = Array.fill(8)(CPIntVar(1 to 4))

  add(x(one) === 1)
  add(x(one + 2) === 1)
  add(x(two) === 2)
  add(x(two + 3) === 2)
  add(x(three) === 3)
  add(x(three + 4) === 3)
  add(x(four) === 4)
  add(x(four + 5) === 4)
  add(gcc(x, 1 to 4, 2, 2))
  add(one < two)

  search {
    binaryFirstFail(Seq(one, two, three, four))
  }

  onSolution {
    println(x.mkString((",")))
  }

  val stats = start()
  println(stats)
}

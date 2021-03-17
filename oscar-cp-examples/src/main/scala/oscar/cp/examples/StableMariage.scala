package oscar.cp.examples

import oscar.cp.{CPIntVar, CPModel, Strong, Weak, add, binaryFirstFail, element, elementVar, onSolution, search, start}

/**
 * Stable Marriage problem:
 * Given n men and n women, where each person has ranked all members of the opposite sex with a unique number between 1 and n in order of preference,
 * marry the men and women together such that there are no two people of opposite sex who would both rather have each other than their current partners.
 * If there are no such people, all the marriages are "stable".
 * Wikipedia: http://en.wikipedia.org/wiki/Stable_marriage_problem
 *
 * @author Hakan Kjellerstrand hakank@gmail.com http://www.hakank.org/oscar/, Pierre Schaus pschaus@gmail.com
 */
object StableMariage extends CPModel with App {

  val n = 5

  val Women = 0 until n
  val Men = 0 until n

  // for each man, what is his ranking for the women (higher is better)
  val rankWomen = Array(
    Array(1, 2, 4, 3, 0),
    Array(1, 3, 2, 0, 4),
    Array(4, 2, 1, 3, 0),
    Array(0, 4, 3, 2, 1),
    Array(3, 2, 1, 0, 4))

  // for each woman, what is her ranking for the men (higher is better)
  val rankMen = Array(
    Array(0, 1, 3, 2, 4),
    Array(2, 3, 0, 4, 1),
    Array(3, 2, 4, 1, 0),
    Array(0, 4, 1, 3, 2),
    Array(4, 1, 2, 0, 3))

  val wife = Array.fill(n)(CPIntVar(Women)) // wife(i) is the woman chosen for man i
  val husband = Array.fill(n)(CPIntVar(Men)) // husband(j) is the man chosen for woman j

  onSolution {
    println("wife   :" + wife.mkString(","))
    println("husband:" + husband.mkString(","))
    println()
  }

  for (m <- Men) {
    add(elementVar(husband, wife(m), m), Strong)
  }
  for (w <- Women) {
    add(elementVar(wife, husband(w), w), Strong)
  }

  for (m <- Men; w <- Women) {
    val pref_m = element(rankMen(m), wife(m), Weak) // preference of m for his wife
    val pref_w = element(rankWomen(w), husband(w), Weak) // preference of w for her husband
    add((pref_m ?> rankMen(m)(w)) ==> (pref_w ?< rankWomen(w)(m)))
    add((pref_w ?> rankWomen(w)(m)) ==> (pref_m ?< rankMen(m)(w)))
  }

  search { binaryFirstFail(wife) }

  val stats = start(nSols = 1)
  println(stats)
}

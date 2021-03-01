package oscar.cp.examples


import oscar.cp._

/**
 * Cookie Monster Problem (by Richard Green https://plus.google.com/u/0/101584889282878921052/posts/8qWvSaLJVGD
 *
 * Suppose that we have a number of cookie jars, each containing a certain number of cookies.
 * The Cookie Monster wants to eat all the cookies, but he is required to do so in a number
 * of sequential moves. At each move, the Cookie Monster (CM) chooses a subset of the jars,
 * and eats the same (nonzero) number of cookies from each jar. The goal of the CM is to
 * empty all the cookies from the jars in the smallest possible number of moves, and the
 * Cookie Monster Problem is to determine this number for any given set of cookie jars.
 *
 * Since the CM has an unlimited appetite, there is essentially no difference between eating
 * three cookies from one jar, and eating three cookies from each of 10 jars. It turns out
 * that there is no advantage to depleting these 10 jars at different rates, so the 10 jars
 * may as well be one jar, and we may as well reduce to the case where no two jars contain
 * the same number of cookies. It is also safe to ignore any empty jars. This means that the
 * starting state of the problem may be described by a set of positive integers, S.
 * The Cookie Monster Number of S, CM(S), is the smallest number of moves in which this set
 * of jars can be completely emptied.
 *
 * Let's look at an example. Suppose that S is the set {15, 13, 12, 4, 2, 1}, meaning that there
 * are six jars, containing 1, 2, 4, 12, 13 and 15 cookies each.
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object CookieMonster extends CPModel with App {


  val jars = Array(15, 13, 12, 4, 2, 1)
  val maxMove = 6

  val x = Array.fill(maxMove)(CPIntVar(0 to jars.max))
  val b = Array.fill(maxMove, jars.size)(CPBoolVar())
  val bx = Array.tabulate(maxMove, jars.size) { case (m, j) => b(m)(j) * x(m) }
  var nbSol = 0

  def printSol() = {
    for (i <- 0 until maxMove) {
      println("move" + i + ":\t" + bx(i).mkString("\t"))
    }
  }

  onSolution {
    printSol()
    nbSol += 1
  }

  for (j <- 0 until jars.size) {
    add(sum(0 until maxMove)(m => bx(m)(j)) === jars(j))
  }
  // break symmetry
  for (m <- 0 until maxMove - 1) {
    add(lexLeq(bx(m + 1), bx(m)))
  }

  search {
    binaryStatic(x) ++ binaryStatic(b.flatten.toSeq)
  }

  for (i <- 0 until maxMove; if nbSol == 0) {
    startSubjectTo(nSols = 1) {
      for (m <- i + 1 until maxMove) {
        if (m > i) post(x(m) === 0)
        else post(x(m) > 0)
      }
    }
  }

  println("nbSol=" + nbSol)

}

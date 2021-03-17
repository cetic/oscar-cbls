package oscar.cp.examples


import oscar.cp._

/**
 * Martin Garner Problem:
 * Let a,b,c,d,e,f,g and h be distinct elements in the set {-7,-5,-3,-2,2,4,6,13}
 * What is the minimum possible value of (a+b+c+d)^2 + (e+f+g+h)^2
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object GardnerSumSquare extends CPModel with App {

  val n = 5
  val dom = Set(-7, -5, -3, -2, 2, 4, 6, 13)

  val a = CPIntVar(dom)
  val b = CPIntVar(dom)
  val c = CPIntVar(dom)
  val d = CPIntVar(dom)
  val e = CPIntVar(dom)
  val f = CPIntVar(dom)
  val g = CPIntVar(dom)
  val h = CPIntVar(dom)
  val s1 = CPIntVar((0 to (dom.max * 4) ^ 2).toSet)
  val s2 = CPIntVar((0 to (dom.max * 4) ^ 2).toSet)
  val obj = (s1 * s1) + (s2 * s2)

  add(allDifferent(Array(a, b, c, d, e, f, g, h)), Strong)
  add(sum(Array(a, b, c, d), s1))
  add(sum(Array(e, f, g, h), s2))
  // break symmetries inside first set
  add(a < b)
  add(b < c)
  add(c < d)
  // break symmetries inside second set
  add(e < f)
  add(f < g)
  add(g < h)
  // break symmetries between the two sets
  add(a < e)

  minimize(obj) search {
    binaryFirstFail(Seq(a, b, c, d, e, f, g, h))
  }

  onSolution {
    println("(a:" + a + " b:" + b + " c:" + c + " d:" + d + ") (e:" + e + " f:" + f + " g:" + g + " h:" + h + ")")
  }

  val stats = start()

  println(stats)
}

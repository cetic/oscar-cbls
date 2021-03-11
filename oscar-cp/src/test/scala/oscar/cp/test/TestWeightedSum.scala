package oscar.cp.test

import org.scalatest.FunSuite
import oscar.cp.testUtils.TestSuite

import oscar.cp._

class TestWeightedSum extends TestSuite {

  test("Weighted Sum 1") {
    val cp = CPSolver()
    var x = Array.tabulate(5)(i => CPIntVar(i)(cp))
    val y = weightedSum(0 until 5) { i => (i, x(i)) }
    // 0*0 + 1*1 + 2*2 + 3*3 + 4*4
    y.value should be(30)
  }

  test("Weighted Sum 2") {
    val cp = CPSolver()
    var x = Array.tabulate(5)(i => CPIntVar(i)(cp))
    val y = weightedSum(0 until 5, x)
    // 0*0 + 1*1 + 2*2 + 3*3 + 4*4
    y.value should be(30)
  }

  test("Weighted Sum 3") {
    val cp = CPSolver()
    var x = Array.tabulate(2, 2)((i, j) => CPIntVar(i * 2 + j)(cp))
    var w = Array.tabulate(2, 2)((i, j) => i * 2 + j)
    // 0*0 + 1*1 + 2*2 + 3*3
    val y = weightedSum(w, x)
    y.value should be(14)
  }

  test("Weighted Sum 4") {
    val cp = CPSolver()
    var x = Array.tabulate(2, 2)((i, j) => CPIntVar(i * 2 + j)(cp))
    var w = Array.tabulate(2, 2)((i, j) => i * 2 + j)
    // 0*0 + 1*1 + 2*2 + 3*3
    val y = weightedSum(0 until x.size, 0 until w.size) { case (i, j) => (w(i)(j), x(i)(j)) }
    y.value should be(14)
  }

  def nbsol(w: Array[Int], domx: Array[Set[Int]], ymin: Int, ymax: Int, decomp: Boolean = false): Int = {
    val cp = CPSolver()
    val x = domx.map(dom => CPIntVar(dom)(cp))
    val y = CPIntVar(ymin to ymax)(cp)
    var n: Int = 0

    if (!decomp)
      cp.add(weightedSum(w, x, y))
    else
      cp.add(sum(w.zip(x).map { case (wi, xi) => xi * wi }) === y)
    cp.search {
      binaryFirstFail(x)
    } onSolution {
      n += 1
    }
    cp.start
    n
  }

  test("Weighted Sum 5") {
    val x = Array(Set(1, 2), Set(4, 6, 8), Set(-6, -4, 0, 6))
    val w = Array(4, -3, 2)
    //println("=>" + nbsol(w, x, -100, 100, true))
    //println("=>" + nbsol(w, x, -100, 100, false))
  }
}
  
  

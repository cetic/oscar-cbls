package oscar.cp.test

import org.scalatest.FunSuite
import oscar.cp.testUtils.TestSuite
import oscar.cp.constraints._
import oscar.cp._

class TestGCC extends TestSuite {

  val rand = new scala.util.Random(0)

  def randomDom(n: Int): Array[Array[Int]] = {
    val low = Array.tabulate(n)(i => rand.nextInt(2))
    val up = Array.tabulate(n)(i => low(i) + rand.nextInt(2))
    Array(low, up)
  }

  def randomOcc(n: Int): Array[Array[Int]] = {
    val low = Array.tabulate(n)(i => rand.nextInt(1))
    val up = Array.tabulate(n)(i => low(i) + rand.nextInt(3))
    Array(low, up)
  }

  /**
   * return the number of sol of the constraints
   */
  def nbSolution(randomDom: Array[Array[Int]], randomOcc: Array[Array[Int]], gccvar: Boolean): Int = {
    val cp = CPSolver()
    val x: Array[CPIntVar] = Array.tabulate(randomDom(0).size)(i => CPIntVar(randomDom(0)(i) to randomDom(1)(i))(cp))
    val o: Array[CPIntVar] = Array.tabulate(randomOcc(0).size)(i => CPIntVar(randomOcc(0)(i) to randomOcc(1)(i))(cp))

    var nb = 0

    val inconsistent = isInconsistent {
      if (gccvar) {
        cp.post(new oscar.cp.constraints.GCCVar(x, -1, o));
      } else {
        cp.post(new oscar.cp.constraints.SoftGCCAC(x, -1, randomOcc(0), randomOcc(1), CPIntVar(0)(cp)));
      }
    }
    if (inconsistent) {
      return -1
    }

    cp.search {
      binaryStatic(x)
    } onSolution {
      if (gccvar) o.forall(_.isBound) should be(true)
      nb += 1
    }
    cp.start()
    nb
  }

  test("GCC1") {
    for (i <- 0 until 150) {
      val randDom = randomDom(3)
      val randOcc = randomOcc(4)
      nbSolution(randDom, randOcc, true) should be(nbSolution(randDom, randOcc, false))
    }
  }

  test("GCC2") {

    val cp = CPSolver()

    val x = Array.fill(10)(CPIntVar(0 to 10)(cp))

    for (i <- 0 until 2; v <- 0 until 5) {
      cp.post(x(i * 5 + v) === v)
    }

    val o = Array.fill(10)(CPIntVar(0 to 10)(cp))

    cp.post(new oscar.cp.constraints.GCCVarAC(x, 0, o));

    cp.isFailed should be(false)

    for (i <- 0 until o.size) {
      o(i).isBound should be(true)
      if (i < 5) o(i).value should be(2)
      else o(i).value should be(0)
    }
  }

  test("GCC3") {

    implicit val cp = CPSolver()
    val n = 2
    val x = Array.fill(n)(CPIntVar(0 to n - 1))
    val allValues = Array.tabulate(n)(i => (i, x(i)))

    assert(isInconsistent(cp.post(gcc(x, allValues), Strong)))
  }
}

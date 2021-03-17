package oscar.cp.constraints

import oscar.algo.reversible.ReversibleInt
import oscar.algo.search.Branching
import oscar.cp.testUtils.TestSuite
import oscar.cp._

import scala.util.Random

class SubCircuitSuite extends TestSuite {

  private def testData(nSuccs: Int): (CPSolver, Array[CPIntVar]) = {
    val cp = CPSolver()
    val succs = Array.fill(nSuccs)(CPIntVar(0 until nSuccs)(cp))
    cp.post(SubCircuit(succs))
    (cp, succs)
  }

  test("one") {
    val (cp, succs) = testData(1)
    assert(succs.head.value == 0)
    assert(!cp.isFailed)
  }

  test("no circuit") {
    val (cp, succs) = testData(10)
    for (i <- 0 until 10) {
      cp.add(succs(i) === i)
    }
    assert(!cp.isFailed)
  }

  test("circuit") {
    val (cp, succs) = testData(5)
    cp.add(succs(0) === 1)
    cp.add(succs(1) === 2)
    cp.add(succs(2) === 3)
    cp.add(succs(3) === 4)
    cp.add(succs(4) === 0)
    assert(!cp.isFailed)
  }

  test("subcircuit 1") {
    val (cp, succs) = testData(5)
    cp.add(succs(0) === 1)
    cp.add(succs(1) === 2)
    cp.add(succs(2) === 0)
    assert(!cp.isFailed)
    assert(succs(3).value == 3)
    assert(succs(4).value == 4)
  }

  test("subcircuit 2") {
    val (cp, succs) = testData(5)
    cp.add(succs(3) === 3)
    cp.add(succs(0) === 1)
    cp.add(succs(1) === 2)
    cp.add(succs(2) === 0)
    assert(!cp.isFailed)
    assert(succs(4).value == 4)
  }

  test("only one subtour") {
    val (cp, succs) = testData(5)
    cp.add(succs(0) === 1)
    cp.add(succs(1) === 2)
    cp.add(succs(3) === 4)
    assert(!cp.isFailed)
    assert(succs(4).value == 0)
    assert(succs(2).value == 3)
  }

  test("issue detected by Damien Mercier") {
    implicit val cp = CPSolver()
    /*
      Two subcircuits :
      - (2->3->4->2)
      - (1->5->1)
      - And elem 0 is not taken (0 -> 0)
     */
    val successors = Array(
      CPIntVar(0), //0
      CPIntVar(5), //1
      CPIntVar(3), //2
      CPIntVar(4), //3
      CPIntVar(2), //4
      CPIntVar(1) //5
    )
    postAndCheckFailure(cp, subCircuit(successors))

    def checkUniqueCircuit(): Boolean = {
      val takenSuccessorsIndices = successors.indices.filter(i => !successors(i).isBoundTo(i))
      val n = takenSuccessorsIndices.length
      if (n == 0) {
        return true
      } //Empty circuit
      val first = takenSuccessorsIndices.head
      var curr = first
      var c = 0
      do {
        curr = successors(curr).value
        c += 1
      } while (curr != first)
      if (c != n) {
        println(takenSuccessorsIndices.map(i => (i, successors(i).value)).mkString(","))
        println(s"Number of successors in one subcircuit:$c")
        println(s"Total number of successors in all subcircuits:$n")
        System.out.flush()
        return false
      }
      return true
    }

    //assert(checkUniqueCircuit())

  }

  test("multiple subcircuit issue") {
    implicit val cp = CPSolver()
    val successors = Array(
      CPIntVar(Set(0)),
      CPIntVar(Set(2)),
      CPIntVar(Set(1)),
      CPIntVar(Set(4)),
      CPIntVar(Set(3))
    )
    postAndCheckFailure(cp, subCircuit(successors))
  }

  test("successors all different") {
    implicit val cp = CPSolver()
    var successors = Array(
      CPIntVar(Set(0)),
      CPIntVar(Set(0)),
      CPIntVar(Set(3)),
      CPIntVar(Set(0)),
      CPIntVar(Set(2))
    )
    postAndCheckFailure(cp, subCircuit(successors))

    successors = Array(
      CPIntVar(Set(4)),
      CPIntVar(Set(0)),
      CPIntVar(Set(4)),
      CPIntVar(Set(3)),
      CPIntVar(Set(1))
    )
    postAndCheckFailure(cp, subCircuit(successors))
  }


  test("multiple subcircuit test (random)") {
    for (i <- 1 to 200) {
      implicit val cp = CPSolver()
      val n = 5
      val succ: Array[Set[Int]] = Array.tabulate(n)(i => Array.fill(Random.nextInt(n - 1) + 1)(Random.nextInt(n)).distinct.toSet[Int])
      val successors: Array[CPIntVar] = Array.tabulate(n)(i => CPIntVar(succ(i)))

      cp.deactivateNoSolExceptions()
      cp.add(subCircuit(successors))
      cp.search(binaryStatic(successors))

      cp.onSolution {
        //On solution, we check if we have 0 or 1 circuit (no more).
        val takenSuccIdx = successors.indices.filter(i => !successors(i).isBoundTo(i))
        val m = takenSuccIdx.length
        if (m != 0) {
          //if not the empty circuit
          val first = takenSuccIdx.head
          var curr = first
          var c = 0
          do {
            curr = successors(curr).value
            c += 1
          } while (curr != first && c <= m)
          if (c != m || curr != first) {
            println(successors.zipWithIndex.map { case (s, i) => (i, s) }.mkString(","))
            System.out.flush()
          }
          assert(c == m, s": $c elements in the subcircuit but $m elements taken.")
          assert(curr == first, s": The last successors ($curr) is not the first one ($first). Not a valid circuit?")
        }
      }

      cp.start()
    }

  }


  test("subcircuit 5") {
    val (cp, succs) = testData(4)
    cp.add(succs(0) === 1)
    postAndCheckFailure(cp, succs(1) === 1)
    assert(cp.isFailed)
  }


  for (n <- 4 to 6)
    test("solve all" + n) {
      //val n = 4
      val (cp, succs) = testData(n)
      cp.search(binaryStatic(succs))
      val stats = cp.start()
      assert(stats.nSols == nSubCircuits(n))
    }

  private def nSubCircuits(n: Int): Int = {
    1 + (2 to n).map(i => combinations(n, i) * factorial(i - 1)).sum
  }

  private def combinations(n: Int, k: Int): Int = {
    factorial(n) / (factorial(n - k) * factorial(k))
  }

  private def factorial(n: Int): Int = {
    if (n == 0) 1
    else factorial(n, 1)
  }

  @annotation.tailrec
  private def factorial(n: Int, cum: Int): Int = {
    if (n == 1) cum
    else factorial(n - 1, cum * n)
  }
}
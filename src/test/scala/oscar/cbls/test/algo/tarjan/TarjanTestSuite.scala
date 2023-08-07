package oscar.cbls.test.algo.tarjan

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.contain
import org.scalatest.matchers.should.Matchers.{convertToAnyShouldWrapper, empty}
import oscar.cbls.algo.tarjan.Tarjan

class TarjanTestSuite extends AnyFunSuite {
  test("Tarjan Empty Graph") {
    val sccG = new Tarjan[Nothing].tarjan(Nil, (_: Nothing) => Nil)
    sccG shouldBe empty
  }

  test("Tarjan Graph without Links") {
    val nodesG = Seq("This", "is", "a", "graph", "without", "links")
    val sccG = new Tarjan[String].tarjan(nodesG, (_: String) => Nil)
    val expectedResult = nodesG.map(s => List(s))
    sccG should contain theSameElementsAs expectedResult
  }

  test("Tarjan Example Graph 1") {
    // Example Graph
    val (a, b, c, d, e, f, g, h, i, j) = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    val nodesG = List(a, b, c, d, e, f, g, h, i, j)
    def adjListG(node: Int): List[Int] = {
      if (node == a) List(b)
      else if (node == b) List(c, d)
      else if (node == c) List(a)
      else if (node == d) List(e)
      else if (node == e) List(f)
      else if (node == f) List(e)
      else if (node == g) List(e, h)
      else if (node == h) List(f, i)
      else if (node == i) List(j)
      else if (node == j) List(g, h)
      else Nil
    }
    // SCCs from graph
    val sccG = new Tarjan[Int].tarjan(nodesG, adjListG)
    val expectedResult = List(
      List(a, b, c), List(d), List(e, f), List(g, h, i, j)
    )
    sccG should contain theSameElementsAs expectedResult
  }

  test("Tarjan Example Graph 2") {
    val nodesG1 = 1 to 20
    val mapAdjG1 = Map(
      1 -> List(2),
      2 -> List(1, 3),
      3 -> List(4, 5, 7, 8),
      4 -> List(),
      5 -> List(6),
      6 -> List(5, 8),
      7 -> List(8),
      8 -> List(9),
      9 -> List(8, 10),
      10 -> List(9),
      11 -> List(7, 12, 14),
      12 -> List(13),
      13 -> List(12),
      14 -> List(14, 15),
      15 -> List(16),
      16 -> List(15, 17),
      17 -> List(16, 18),
      18 -> List(19),
      19 -> List(20),
      20 -> List(15)
    )
    val adjListG1 = (node: Int) => { mapAdjG1(node) }
    val sccG = new Tarjan[Int].tarjan(nodesG1, adjListG1)
    val expectedResult = List(
      List(1, 2),
      List(3),
      List(4),
      List(5, 6),
      List(7),
      List(8, 9, 10),
      List(11),
      List(12, 13),
      List(14),
      List(15, 16, 17, 18, 19, 20)
    )
    sccG should contain theSameElementsAs expectedResult
  }

  test("Tarjan Example Graph 3") {
    val nodesG1 = 1 to 15
    val mapAdjG1 = Map(
      1 -> List(2, 4),
      2 -> List(3),
      3 -> List(1),
      4 -> List(5, 9),
      5 -> List(6, 8),
      6 -> List(7),
      7 -> List(5, 8),
      8 -> List(),
      9 -> List(10, 13),
      10 -> List(11),
      11 -> List(12),
      12 -> List(10),
      13 -> List(14, 15),
      14 -> List(15),
      15 -> List(4)
    )
    val adjListG1 = (node: Int) => { mapAdjG1(node) }
    val sccG = new Tarjan[Int].tarjan(nodesG1, adjListG1)
    val expectedResult = List(
      List(1, 2, 3),
      List(5, 6, 7),
      List(8),
      List(4, 9, 13, 14, 15),
      List(10, 11, 12)
    )
    sccG should contain theSameElementsAs expectedResult
  }
}

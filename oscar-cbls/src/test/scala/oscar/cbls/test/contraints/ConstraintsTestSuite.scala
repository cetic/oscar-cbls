package oscar.cbls.test.contraints

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import oscar.cbls.intToLong
import oscar.cbls.lib.constraint._
import oscar.cbls.test.invariants.bench._

import scala.collection.immutable.SortedMap

class ConstraintsTestSuite extends FunSuite with Checkers{

  val verbose = 0

  test("Disjunctive maintains the overlap between tasks on unary resource.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    Disjunctive(bench.genIntVarsArray(),bench.genIntVarsArray())
    bench.run()
  }

  test("DisjunctiveWithTransitions maintains the overlap between tasks on unary resource.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    val size = 5
    DisjunctiveWithTransitions(bench.genIntVarsArray(size),bench.genIntVarsArray(size),Array.fill(size)(Array.fill(size)(scala.util.Random.nextLong())))
    bench.run()
  }

  test("DisjunctiveConstDuration maintains the overlap between tasks on unary resource.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    val size = 4
    DisjunctiveConstDuration(bench.genIntVarsArray(size,0 to 100),Array.fill(size)(intToLong(scala.util.Random.nextInt(15))))
    bench.run()
  }

  test("AllDiff maintains output = all int vars have different values") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    AllDiff(bench.genIntVarsArray(10, -10 to 10))
    bench.run()
  }

  test("AtLeast") {
    def myMapValues[B,C](s:SortedMap[Long,B],f:B=>C):SortedMap[Long,C] =
      s.foldLeft[SortedMap[Long,C]](SortedMap.empty)((acc,couple) => acc+((couple._1,f(couple._2))))

    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    val constantMap = InvGen.randomIntSortedMap(10, 0 to 30, 0 to 30)
    val c = AtLeast(bench.genIntVars(10), myMapValues(constantMap, (_:Long) => bench.genIntVar(0 to 30)))
    bench.run(c)
  }

  test("AtMost") {

    def myMapValues[B,C](s:SortedMap[Long,B],f:B=>C):SortedMap[Long,C] =
      s.foldLeft[SortedMap[Long,C]](SortedMap.empty)((acc,couple) => acc+((couple._1,f(couple._2))))

    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    val constantMap = InvGen.randomIntSortedMap(10, 0 to 30, 0 to 30)
    val c = AtMost(bench.genIntVars(10, range = 0 to 30),myMapValues(constantMap, (_:Long) => bench.genIntVar(0 to 30)))

    bench.run(c)
  }
}

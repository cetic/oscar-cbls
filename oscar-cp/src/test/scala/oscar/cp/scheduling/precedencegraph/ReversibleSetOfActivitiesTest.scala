package oscar.cp.scheduling.precedencegraph

import org.scalatest.{Assertions, FunSuite, Matchers}
import oscar.cp.core.CPSolver
import oscar.cp.scheduling.precedencegraph.datastructures.ReversibleSetOfActivities

/**
  * Created by saschavancauwelaert on 13/09/2017.
  */
class ReversibleSetOfActivitiesTest extends FunSuite with Matchers with Assertions {

  test("Test 1") {

    val cp = CPSolver()
    val test = new ReversibleSetOfActivities(cp, 5)
    //println(test)

    cp.pushState()
    test.add(2)
    assert(test.hasValue(2))
    //println(test)
    cp.pushState()
    test.add(2)
    test.add(4)
    assert(test.hasValue(2))
    assert(test.hasValue(4))
    //println(test)
    cp.pop()
    //println(test)
    cp.pop()
    //println(test)
    for(i <- 0 until 5) {
      test.add(i)
      assert(test.hasValue(i))
    }

  }

}

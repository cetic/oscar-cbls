package oscar.cp.constraints

import oscar.cp._
import oscar.cp.testUtils._
import oscar.cp.core.CPSolver
import oscar.cp.core.variables.CPIntVar


class IntDivisionACSuite extends TestSuite {
  
  test("an IllegalArgumentException should be thrown if c <= 0") {
    implicit val solver = CPSolver()
    val a = CPIntVar(1 to 10)
    val b = CPIntVar(1 to 10)
    intercept[IllegalArgumentException] {
      new IntDivisionAC(a, b, 0)
    }
    intercept[IllegalArgumentException] {
      new IntDivisionAC(a, b, -10)
    }
  }
  
  test("values of a that have initially no support in b should be removed") {
    implicit val solver = CPSolver()
    val values = (1 to 10)
    val a = CPIntVar(values)
    val b = CPIntVar(Set(2, 10, 11, 13, 15, 17, 34))
    solver.post(new IntDivisionAC(a, b, 3))
    assert(!solver.isFailed)
    val contained = Array(3, 4, 5)
    val removed = values.filter(!contained.contains(_))
    for (v <- contained) {
      assert(a.hasValue(v), s"$v should be in $a")
    }
    for (v <- removed) {
      assert(!a.hasValue(v), s"$v should not be in $a")
    }
  }
  
  test("values of b that have initially no support in a should be removed") {
    implicit val solver = CPSolver()
    val values = (0 to 20)
    val a = CPIntVar(Set(1, 4, 5, 9))
    val b = CPIntVar(values)
    solver.post(new IntDivisionAC(a, b, 3))
    assert(!solver.isFailed)
    val contained = Set(3, 4, 5, 12, 13, 14, 15, 16, 17)
    val removed = values.filter(!contained.contains(_))
    for (v <- contained) {
      assert(b.hasValue(v), s"$v should be in $b")
    }
    for (v <- removed) {
      assert(!b.hasValue(v), s"$v should not be in $b")
    }
  }
  
  test("a should be equal to b if c equals 1") {
    implicit val solver = CPSolver()
    val a = CPIntVar(-10 to 10)
    val b = CPIntVar(-5 to 15)
    solver.post(new IntDivisionAC(a, b, 1)) 
    assert(!solver.isFailed)
    for (v <- a) {
      assert(b.hasValue(v), s"$v should be in $b")
    }
    for (v <- b) {     
      assert(a.hasValue(v), s"$v should be in $a")
    }
  }
  
  test("b should be reduced to [-c+1; c-1] if a is initially equal to 0") {
    implicit val solver = CPSolver()
    val values = -10 to 10
    val a = CPIntVar(0)
    val b = CPIntVar(values)
    val c = 3
    solver.post(new IntDivisionAC(a, b, c)) 
    assert(!solver.isFailed)
    val contained = ((-c+1) until c).toSet
    val removed = values.filter(!contained.contains(_))
    for (v <- contained) {
      assert(b.hasValue(v), s"$v should be in $b")
    }
    for (v <- removed) {
      assert(!b.hasValue(v), s"$v should not be in $b")
    }
  }
  
  test("the constraint should fail if a has no support in b") {
    implicit val solver = CPSolver()
    val a = CPIntVar(Set(1, 4, 5, 9))
    val b = CPIntVar(Set(2, 6, 7, 11, 18))
    postAndCheckFailure(solver, new IntDivisionAC(a, b, 3))
  }
  
  test("values in [v*c; v*c+c[ should be removed from b when v is removed from a") {
    implicit val solver = CPSolver()
    val values = -15 to 15
    val a = CPIntVar(-5 to 5)
    val b = CPIntVar(values)
    val c = 3
    solver.post(new IntDivisionAC(a, b, c)) 
    assert(!solver.isFailed)
    def removeAndCheck(v: Int): Unit = {     
      solver.post(a !== v)
      for (i <- (v*c) until (v*c+c)) {
        assert(!b.hasValue(i), s"$i should not be in $b")
      }
    }
    removeAndCheck(-4)
    removeAndCheck(0)
    removeAndCheck(3)
  } 
  
  test("v/c should be removed from a when v is removed from b and v is the only support of v/c") {
    implicit val solver = CPSolver()
    val a = CPIntVar(1 to 5)
    val b = CPIntVar(Set(1, 13, 20))
    val c = 5
    solver.post(new IntDivisionAC(a, b, c)) 
    assert(!solver.isFailed)
    solver.post(b !== 13)
    assert(!a.hasValue(2), s"2 should not be in $a")
  } 
  
  test("removing v from b should not remove v/4 from a if v/4 has still some supports in b") {
    implicit val solver = CPSolver()
    val a = CPIntVar(1 to 5)
    val b = CPIntVar(1 to 15)
    val c = 5
    solver.post(new IntDivisionAC(a, b, c)) 
    assert(!solver.isFailed)
    solver.post(b !== 12)
    assert(a.hasValue(2), s"2 should be in $a")
    assert(b.hasValue(10), s"10 should be in $b")
    assert(b.hasValue(11), s"11 should be in $b")
    assert(!b.hasValue(12), s"12 should not be in $b")
    assert(b.hasValue(13), s"13 should be in $b")
    assert(b.hasValue(14), s"14 should be in $b")
    assert(b.hasValue(15), s"15 should be in $b")
  }
}
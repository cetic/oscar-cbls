package oscar.cp.core.variables

import oscar.cp.testUtils._
import oscar.cp.core.CPOutcome._
import scala.util.Random
import oscar.cp.core.CPStore

class CPIntVarAdaptableSuite extends TestSuite {

  // Returns true if the domain contains all the values between min and max
  private def containsAll(variable: CPIntVar): Boolean = {
    val min = variable.min
    val max = variable.max
    (min to max).forall(variable.hasValue)
  }

  test("All values should be contained in the initial domain") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 10, true)
    assert(variable.size == 6)
    variable shouldContain (5 to 10)
  }

  test("HasValue should return true if value is in the domain") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    variable shouldContain 5
    variable shouldContain 10
    variable shouldContain 15
  }

  test("HasValue should return false if value is not in the domain") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    assert(!variable.hasValue(-1000))
    assert(!variable.hasValue(-10))
    assert(!variable.hasValue(4))
    assert(!variable.hasValue(16))
    assert(!variable.hasValue(20))
    assert(!variable.hasValue(1000))
  }

  test("UpdateMin should adjust the minimum value and the size") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    assert(variable.size == 11)
    assert(variable.updateMin(10) == Suspend)
    assert(variable.size == 6)
    assert(variable.min == 10)
  }

  test("UpdateMin should remove all values lesser than min") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    assert(variable.updateMin(10) == Suspend)
    assert(containsAll(variable))
  }

  test("UpdateMin with a lesser or equal value than min should not impact the domain") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    assert(variable.updateMin(4) == Suspend)
    assert(variable.size == 11)
    assert(variable.min == 5)
    assert(variable.max == 15)
    assert(variable.updateMin(5) == Suspend)
    assert(variable.size == 11)
    assert(variable.min == 5)
    assert(variable.max == 15)
  }

  test("UpdateMin to max should assign max") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    assert(variable.updateMin(15) == Suspend)
    assert(variable.size == 1)
    assert(variable.isBound)
    assert(variable.hasValue(15))
  }

  test("UpdateMin greater than max should fail") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    assert(variable.updateMin(20) == Failure)
    //intercept[Inconsistency](variable.updateMin(20))
  }

  test("UpdateMax should adjust the maximum value and the size") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    assert(variable.size == 11)
    assert(variable.updateMax(10) == Suspend)
    assert(variable.size == 6)
    assert(variable.max == 10)
  }

  test("UpdateMax should remove all values greater than max") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    assert(variable.updateMax(10) == Suspend)
    assert(containsAll(variable))
  }

  test("UpdateMax with a greater or equal value than max should not impact the domain") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    assert(variable.updateMax(20) == Suspend)
    assert(variable.size == 11)
    assert(variable.min == 5)
    assert(variable.max == 15)
    assert(variable.updateMax(15) == Suspend)
    assert(variable.size == 11)
    assert(variable.min == 5)
    assert(variable.max == 15)
  }

  test("UpdateMax to min should assign min") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    assert(variable.updateMax(5) == Suspend)
    assert(variable.size == 1)
    assert(variable.isBound)
    assert(variable.hasValue(5))
  }

  test("UpdateMax lesser than min should fail") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    assert(variable.updateMax(0) == Failure)
    //intercept[Inconsistency](variable.updateMax(0))
  }

  test("Bounds should be restored when a backtrack occurs") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    store.pushState()
    assert(variable.updateMax(10) == Suspend)
    assert(variable.min == 5)
    assert(variable.max == 10)
    assert(variable.size == 6)
    assert(containsAll(variable))
    store.pushState()
    assert(variable.updateMax(9) == Suspend)
    assert(variable.updateMin(6) == Suspend)
    assert(variable.min == 6)
    assert(variable.max == 9)
    assert(variable.size == 4)
    assert(containsAll(variable))
    store.pushState()
    store.pop()
    assert(variable.min == 6)
    assert(variable.max == 9)
    assert(variable.size == 4)
    assert(containsAll(variable))
    store.pop()
    assert(variable.min == 5)
    assert(variable.max == 10)
    assert(variable.size == 6)
    assert(containsAll(variable))
    store.pop()
    assert(variable.min == 5)
    assert(variable.max == 15)
    assert(variable.size == 11)
    assert(containsAll(variable))
  }

  test("Assign should make min equal to max") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    assert(variable.assign(10) == Suspend)
    assert(variable.hasValue(10))
    assert(variable.min == 10)
    assert(variable.max == 10)
  }

  test("Assign should reduce the size to 1") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    assert(variable.assign(10) == Suspend)
    assert(variable.size == 1)
  }

  test("Assign an out of bounds value should fail") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    assert(variable.assign(20) == Failure)
    //intercept[Inconsistency](variable.assign(20))
  }

  test("Random values should be contained in the domain") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 10, 30, true)
    for (seed <- 1 to 10) {
      val rand = new Random(seed)
      for (i <- 1 to 20) {
        val value = variable.randomValue(rand)
        assert(variable.hasValue(value), s"$value is not in the domain")
      }
    }
  }

  test("Random values should always be the assigned value when the size is 1") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 10, 10, true)
    for (seed <- 1 to 10) {
      val rand = new Random(seed)
      for (i <- 1 to 20) {
        val value = variable.randomValue(rand)
        assert(value == 10, s"$value is not the assigned value")
      }
    }
  }

  test("Iterator should iterate on all the values") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    val values1 = (5 to 15).toSet
    assert(variable.iterator.forall(variable.hasValue(_)))
    assert(variable.iterator.forall(values1.contains(_)))
    assert(variable.iterator.size == 11)
    val values2 = (7 to 10).toSet
    assert(variable.updateMin(7) == Suspend)
    assert(variable.updateMax(10) == Suspend)
    assert(variable.size == 4)
    assert(variable.iterator.forall(variable.hasValue(_)))
    assert(variable.iterator.forall(values2.contains(_)))
    assert(variable.iterator.size == 4)
  }

  test("Removed values should not be contained in the domain anymore") {
    val context = new CPStore()
    val domain = new CPIntVarAdaptable(context, 5, 10, true)
    assert(domain.removeValue(5) == Suspend)
    assert(!domain.hasValue(5))
    assert(domain.removeValue(7) == Suspend)
    assert(!domain.hasValue(7))
    assert(domain.removeValue(8) == Suspend)
    assert(!domain.hasValue(8))
  }

  test("Remove a value should reduce the size") {
    val context = new CPStore()
    val domain = new CPIntVarAdaptable(context, 5, 10, true)
    val size = domain.size
    assert(domain.removeValue(5) == Suspend)
    assert(domain.size == size - 1)
    assert(domain.removeValue(5) == Suspend)
    assert(domain.size == size - 1)
    assert(domain.removeValue(6) == Suspend)
    assert(domain.size == size - 2)
  }

  test("Remove a removed value should not impact the domain") {
    val context = new CPStore()
    val domain = new CPIntVarAdaptable(context, 5, 10, true)
    val size = domain.size
    assert(domain.removeValue(4) == Suspend)
    assert(domain.size == size)
    assert(domain.removeValue(11) == Suspend)
    assert(domain.size == size)

  }

  test("Remove the minimal value should change the minimum value") {
    val context = new CPStore()
    val domain = new CPIntVarAdaptable(context, 5, 10, true)
    val size = domain.size
    assert(domain.removeValue(5) == Suspend)
    assert(domain.min == 6)
    assert(domain.removeValue(6) == Suspend)
    assert(domain.removeValue(7) == Suspend)
    assert(domain.min == 8)
    assert(domain.removeValue(10) == Suspend)
    assert(domain.min == 8)
  }

  test("Remove all but one value should assign that value") {
    val context = new CPStore()
    val domain = new CPIntVarAdaptable(context, 5, 10, true)
    val size = domain.size
    assert(domain.removeValue(5) == Suspend)
    assert(domain.hasValue(7))
    assert(domain.removeValue(6) == Suspend)
    assert(domain.hasValue(7))
    assert(domain.removeValue(9) == Suspend)
    assert(domain.hasValue(7))
    assert(domain.removeValue(10) == Suspend)
    assert(domain.hasValue(7))
    assert(domain.removeValue(8) == Suspend)
    assert(domain.hasValue(7))
    assert(domain.isBound)
  }

  test("Removed values should be restored when a backtrack occurs") {
    val context = new CPStore()
    val domain = new CPIntVarAdaptable(context, 5, 10, true)
    val size = domain.size
    context.pushState()
    assert(domain.removeValue(5) == Suspend)
    assert(domain.removeValue(6) == Suspend)
    context.pushState()
    assert(domain.removeValue(9) == Suspend)
    context.pushState()
    assert(domain.removeValue(8) == Suspend)
    assert(!domain.hasValue(5))
    assert(!domain.hasValue(6))
    assert(domain.hasValue(7))
    assert(!domain.hasValue(8))
    assert(!domain.hasValue(9))
    assert(domain.hasValue(10))
    context.pop()
    assert(!domain.hasValue(5))
    assert(!domain.hasValue(6))
    assert(domain.hasValue(7))
    assert(domain.hasValue(8))
    assert(!domain.hasValue(9))
    assert(domain.hasValue(10))
    context.pop()
    assert(!domain.hasValue(5))
    assert(!domain.hasValue(6))
    assert(domain.hasValue(7))
    assert(domain.hasValue(8))
    assert(domain.hasValue(9))
    assert(domain.hasValue(10))
    context.pop()
    assert(domain.hasValue(5))
    assert(domain.hasValue(6))
    assert(domain.hasValue(7))
    assert(domain.hasValue(8))
    assert(domain.hasValue(9))
    assert(domain.hasValue(10))
  }

  test("Remove the assigned value should fail") {
    val context = new CPStore()
    val domain = new CPIntVarAdaptable(context, 10, 10, true)
    assert(domain.removeValue(10) == Failure)
  }

  test("Iterator should iterate on all the values (sparse)") {
    val context = new CPStore()
    val values = Set(10, 11, 15, 16, 17, 20, 21, 25)
    val domain = new CPIntVarAdaptable(context, 10, 25, true)
    (10 to 25).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    println("iterator " + domain.iterator.mkString(" "))
    assert(domain.iterator.size == 8)
    assert(domain.iterator.min == 10)
    assert(domain.iterator.max == 25)
    assert(domain.iterator.forall(values.contains))
  }

  test("UpdateMin should adjust the minimum value and the size (sparse)") {
    val context = new CPStore()
    val values = Set(10, 11, 15, 16, 17, 20, 21, 25)
    val domain = new CPIntVarAdaptable(context, 10, 25, true)
    (10 to 25).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    assert(domain.updateMin(12) == Suspend)
    assert(domain.size == 6)
    assert(domain.min == 15)
  }

  test("UpdateMin should remove all values lesser than min (sparse)") {
    val context = new CPStore()
    val values = Set(10, 11, 15, 16, 17, 20, 21, 25)
    val domain = new CPIntVarAdaptable(context, 10, 25, true)
    (10 to 25).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    assert(domain.updateMin(16) == Suspend)
    assert(!domain.hasValue(10))
    assert(!domain.hasValue(11))
    assert(!domain.hasValue(15))
  }

  test("UpdateMax should adjust the maximum value and the size (sparse)") {
    val context = new CPStore()
    val values = Set(10, 11, 15, 16, 17, 20, 21, 25)
    val domain = new CPIntVarAdaptable(context, 10, 25, true)
    (10 to 25).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    assert(domain.updateMax(19) == Suspend)
    assert(domain.size == 5)
    assert(domain.max == 17)
  }

  test("UpdateMax should remove all values greater than max (sparse)") {
    val context = new CPStore()
    val values = Set(10, 11, 15, 16, 17, 20, 21, 25)
    val domain = new CPIntVarAdaptable(context, 10, 25, true)
    (10 to 25).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    assert(domain.updateMax(17) == Suspend)
    assert(!domain.hasValue(20))
    assert(!domain.hasValue(21))
    assert(!domain.hasValue(25))
  }
  
  
  test("Copyt domain and to Array") {
    val context = new CPStore()
    val values = Set(10, 11, 15, 16, 17, 20, 21, 25)
    val domain = new CPIntVarAdaptable(context, 10, 25, true)
    (10 to 25).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    val valuesArray = Array.ofDim[Int](values.size)
    val s = domain.fillArray(valuesArray)
    assert(s == values.size)
    assert(valuesArray.toSet == values)
    assert(domain.toArray.toSet == values)
    
  }
  
}
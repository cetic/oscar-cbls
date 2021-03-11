package oscar.cp.core.variables

import oscar.cp.core.{CPPropagStrength, CPStore, Constraint}
import oscar.cp.isInconsistent
import oscar.cp.testUtils._

import scala.util.Random

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
    variable.updateMin(10)
    assert(variable.size == 6)
    assert(variable.min == 10)
  }

  test("UpdateMin should remove all values lesser than min") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    variable.updateMin(10)
    assert(containsAll(variable))
  }

  test("UpdateMin with a lesser or equal value than min should not impact the domain") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    variable.updateMin(4)
    assert(variable.size == 11)
    assert(variable.min == 5)
    assert(variable.max == 15)
    variable.updateMin(5)
    assert(variable.size == 11)
    assert(variable.min == 5)
    assert(variable.max == 15)
  }

  test("UpdateMin to max should assign max") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    variable.updateMin(15)
    assert(variable.size == 1)
    assert(variable.isBound)
    assert(variable.hasValue(15))
  }

  test("UpdateMin greater than max should fail") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    assert(isInconsistent(variable.updateMin(20) ))
    //intercept[Inconsistency](variable.updateMin(20))
  }

  test("UpdateMax should adjust the maximum value and the size") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    assert(variable.size == 11)
    variable.updateMax(10)
    assert(variable.size == 6)
    assert(variable.max == 10)
  }

  test("UpdateMax should remove all values greater than max") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    variable.updateMax(10)
    assert(containsAll(variable))
  }

  test("UpdateMax with a greater or equal value than max should not impact the domain") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    variable.updateMax(20)
    assert(variable.size == 11)
    assert(variable.min == 5)
    assert(variable.max == 15)
    variable.updateMax(15)
    assert(variable.size == 11)
    assert(variable.min == 5)
    assert(variable.max == 15)
  }

  test("UpdateMax to min should assign min") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    variable.updateMax(5)
    assert(variable.size == 1)
    assert(variable.isBound)
    assert(variable.hasValue(5))
  }

  test("UpdateMax lesser than min should fail") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    assert(isInconsistent(variable.updateMax(0) ))
    //intercept[Inconsistency](variable.updateMax(0))
  }

  test("Bounds should be restored when a backtrack occurs") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    store.pushState()
    variable.updateMax(10)
    assert(variable.min == 5)
    assert(variable.max == 10)
    assert(variable.size == 6)
    assert(containsAll(variable))
    store.pushState()
    variable.updateMax(9)
    variable.updateMin(6)
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
    variable.assign(10)
    assert(variable.hasValue(10))
    assert(variable.min == 10)
    assert(variable.max == 10)
  }

  test("Assign should reduce the size to 1") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    variable.assign(10)
    assert(variable.size == 1)
  }

  test("Assign an out of bounds value should fail") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 5, 15, true)
    assert(isInconsistent(variable.assign(20) ))
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
    variable.updateMin(7)
    variable.updateMax(10)
    assert(variable.size == 4)
    assert(variable.iterator.forall(variable.hasValue(_)))
    assert(variable.iterator.forall(values2.contains(_)))
    assert(variable.iterator.size == 4)
  }

  test("Removed values should not be contained in the domain anymore") {
    val context = new CPStore()
    val domain = new CPIntVarAdaptable(context, 5, 10, true)
    domain.removeValue(5)
    assert(!domain.hasValue(5))
    domain.removeValue(7)
    assert(!domain.hasValue(7))
    domain.removeValue(8)
    assert(!domain.hasValue(8))
  }

  test("Remove a value should reduce the size") {
    val context = new CPStore()
    val domain = new CPIntVarAdaptable(context, 5, 10, true)
    val size = domain.size
    domain.removeValue(5)
    assert(domain.size == size - 1)
    domain.removeValue(5)
    assert(domain.size == size - 1)
    domain.removeValue(6)
    assert(domain.size == size - 2)
  }

  test("Remove a removed value should not impact the domain") {
    val context = new CPStore()
    val domain = new CPIntVarAdaptable(context, 5, 10, true)
    val size = domain.size
    domain.removeValue(4)
    assert(domain.size == size)
    domain.removeValue(11)
    assert(domain.size == size)

  }

  test("Remove the minimal value should change the minimum value") {
    val context = new CPStore()
    val domain = new CPIntVarAdaptable(context, 5, 10, true)
    val size = domain.size
    domain.removeValue(5)
    assert(domain.min == 6)
    domain.removeValue(6)
    domain.removeValue(7)
    assert(domain.min == 8)
    domain.removeValue(10)
    assert(domain.min == 8)
  }

  test("Remove all but one value should assign that value") {
    val context = new CPStore()
    val domain = new CPIntVarAdaptable(context, 5, 10, true)
    val size = domain.size
    domain.removeValue(5)
    assert(domain.hasValue(7))
    domain.removeValue(6)
    assert(domain.hasValue(7))
    domain.removeValue(9)
    assert(domain.hasValue(7))
    domain.removeValue(10)
    assert(domain.hasValue(7))
    domain.removeValue(8)
    assert(domain.hasValue(7))
    assert(domain.isBound)
  }

  test("Removed values should be restored when a backtrack occurs") {
    val context = new CPStore()
    val domain = new CPIntVarAdaptable(context, 5, 10, true)
    val size = domain.size
    context.pushState()
    domain.removeValue(5)
    domain.removeValue(6)
    context.pushState()
    domain.removeValue(9)
    context.pushState()
    domain.removeValue(8)
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
    assert(isInconsistent(domain.removeValue(10) ))
  }

  test("Iterator should iterate on all the values (sparse)") {
    val context = new CPStore()
    val values = Set(10, 11, 15, 16, 17, 20, 21, 25)
    val domain = new CPIntVarAdaptable(context, 10, 25, true)
    (10 to 25).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    //println("iterator " + domain.iterator.mkString(" "))
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
    domain.updateMin(12)
    assert(domain.size == 6)
    assert(domain.min == 15)
  }

  test("UpdateMin should remove all values lesser than min (sparse)") {
    val context = new CPStore()
    val values = Set(10, 11, 15, 16, 17, 20, 21, 25)
    val domain = new CPIntVarAdaptable(context, 10, 25, true)
    (10 to 25).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    domain.updateMin(16)
    assert(!domain.hasValue(10))
    assert(!domain.hasValue(11))
    assert(!domain.hasValue(15))
  }

  test("UpdateMax should adjust the maximum value and the size (sparse)") {
    val context = new CPStore()
    val values = Set(10, 11, 15, 16, 17, 20, 21, 25)
    val domain = new CPIntVarAdaptable(context, 10, 25, true)
    (10 to 25).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    domain.updateMax(19)
    assert(domain.size == 5)
    assert(domain.max == 17)
  }

  test("UpdateMax should remove all values greater than max (sparse)") {
    val context = new CPStore()
    val values = Set(10, 11, 15, 16, 17, 20, 21, 25)
    val domain = new CPIntVarAdaptable(context, 10, 25, true)
    (10 to 25).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    domain.updateMax(17)
    assert(!domain.hasValue(20))
    assert(!domain.hasValue(21))
    assert(!domain.hasValue(25))
  }
  
  
  test("Copy domain and to Array") {
    val context = new CPStore()
    val values = Set(10, 11, 15, 16, 17, 20, 21, 25)
    val domain = new CPIntVarAdaptable(context, 5, 30, true)
    domain.updateMin(10)
    domain.updateMax(25)
    assert(domain.toArray.toSet == (10 to 25).toSet)
    (10 to 25).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    val valuesArray = Array.ofDim[Int](values.size)
    val s = domain.fillArray(valuesArray)
    assert(s == values.size)
    assert(valuesArray.toSet == values)
    assert(domain.toArray.toSet == values)
  }
  
  test("Copy domain and to Array with pop") {
    val context = new CPStore()
    val values = Set(2,3,5,7)
    val domain = new CPIntVarAdaptable(context, 2, 7, true)
    assert(domain.toArray.toSet == (2 to 7).toSet)
    (2 to 7).foreach(v => if (!values.contains(v)) domain.removeValue(v))
    
    domain.removeValue(3)
    
    context.pushState()
    domain.updateMax(4)
    
    assert(domain.size == 1)
    assert(domain.toArray.size == 1)
    assert(!domain.hasValue(5))
    
    context.pop()
    
    assert(domain.hasValue(5))
    
    val valuesArray = Array.ofDim[Int](values.size)
    val s = domain.fillArray(valuesArray)
    assert(s == 3)
    assert(valuesArray.take(s).toSet == Set(2,5,7))
    assert(domain.toArray.toSet == Set(2,5,7))   
  }  
  
  test("Restrict should restrict the domain") {
    val context = new CPStore()
    val variable = new CPIntVarAdaptable(context, 0, 10, true)
    val values = Array(1, 6, 8, 7)
    variable.restrict(values, 4)
    variable shouldContain values
    assert(variable.size == 4)
    variable.restrict(values, 2)
    variable shouldContain 1
    variable shouldContain 6
    assert(variable.size == 2)
    variable.restrict(values, 1)
    variable shouldContain 1
    assert(variable.size == 1)
  }
  
  test("Restrict should recompute min and max.") {
    val context = new CPStore()
    val variable = new CPIntVarAdaptable(context, 0, 10, true)
    val values = Array(6, 3, 8, 2)
    variable.restrict(values, 4)
    assert(variable.min == 2)
    assert(variable.max == 8)
    variable.restrict(values, 2)
    assert(variable.min == 3)
    assert(variable.max == 6)
    variable.restrict(values, 1)
    assert(variable.min == 6)
    assert(variable.max == 6)
  }
  
  test("The domain should be correctly restored after using restrict") {
    val context = new CPStore()
    val variable = new CPIntVarAdaptable(context, 0, 10, true)
    val values1 = Array(1, 7, 6, 8, 3)
    val values2 = Array(1, 7, 6)
    val values3 = Array(1)
    context.pushState()
    variable.restrict(values1, values1.length)
    context.pushState()
    variable.restrict(values2, values2.length)
    context.pushState()
    variable.restrict(values3, values3.length)
    variable shouldContain values3
    assert(variable.size == 1)
    context.pop()
    variable shouldContain values2
    assert(variable.size == 3)
    context.pop()
    variable shouldContain values1
    assert(variable.size == 5)
    context.pop()
    variable shouldContain (0 to 10)
  }
  
  test("Restrict should notify removed value events") { 
    
    val context = new CPStore()
    val variable = new CPIntVarAdaptable(context, 0, 10, true)
    val removedValues = scala.collection.mutable.Set[Int]()
    
    class TestConstraint extends Constraint(context, "valRemoveTester") {
      override def setup(l: CPPropagStrength): Unit = {
        variable.callValRemoveWhenValueIsRemoved(this)
      }
      override def valRemove(x: CPIntVar, value: Int): Unit = {
        removedValues.add(value)
      }
      override def associatedVars(): Iterable[CPVar] = ???
    }
    
    context.add(new TestConstraint)
    val values1 = Array(1, 7, 6, 8, 3)
    val values2 = Array(1, 7, 6)
    val values3 = Array(1)
    variable.restrict(values1, values1.length)
    context.propagate()
    assert(removedValues == Set(0, 2, 4, 5, 9, 10))
    variable.restrict(values2, values2.length)
    context.propagate()
    assert(removedValues == Set(0, 2, 3, 4, 5, 8, 9, 10))
    variable.restrict(values3, values3.length)
    context.propagate()
    assert(removedValues == Set(0, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  }
  
  test("Restrict should notify on domain events") { 
    
    val context = new CPStore()
    val variable = new CPIntVarAdaptable(context, 0, 10, true)
    var n = 0
    
    class TestConstraint extends Constraint(context, "valRemoveTester") {
      override def setup(l: CPPropagStrength): Unit = {
        variable.callPropagateWhenDomainChanges(this)
      }
      override def propagate(): Unit = {
        n += 1
      }
      override def associatedVars(): Iterable[CPVar] = ???
    }
    
    context.add(new TestConstraint)
    val values1 = Array(1, 7, 6, 8, 3)
    val values2 = Array(1, 7, 6)
    val values3 = Array(1)
    variable.restrict(values1, values1.length)
    context.propagate()
    assert(n == 1)
    variable.restrict(values2, values2.length)
    context.propagate()
    assert(n == 2)
    variable.restrict(values3, values3.length)
    context.propagate()
    assert(n == 3)
  }
  
  test("Restrict should notify on bind events") { 
    
    val context = new CPStore()
    val variable = new CPIntVarAdaptable(context, 0, 10, true)
    var n = 0
    
    class TestConstraint extends Constraint(context, "valRemoveTester") {
      override def setup(l: CPPropagStrength): Unit = {
        variable.callPropagateWhenBind(this)
      }
      override def propagate(): Unit = {
        n += 1
      }
      override def associatedVars(): Iterable[CPVar] = ???
    }
    
    context.add(new TestConstraint)
    val values1 = Array(1, 7, 6, 8, 3)
    val values2 = Array(1, 7, 6)
    val values3 = Array(1)
    variable.restrict(values1, values1.length)
    context.propagate()
    assert(n == 0)
    variable.restrict(values2, values2.length)
    context.propagate()
    assert(n == 0)
    variable.restrict(values3, values3.length)
    context.propagate()
    assert(n == 1)
  }
  
  test("Restrict should notify on bound events") { 
    
    val context = new CPStore()
    val variable = new CPIntVarAdaptable(context, 0, 10, true)
    var n = 0
    
    class TestConstraint extends Constraint(context, "valRemoveTester") {
      override def setup(l: CPPropagStrength): Unit = {
        variable.callPropagateWhenBoundsChange(this)
      }
      override def propagate(): Unit = {
        n += 1
      }
      override def associatedVars(): Iterable[CPVar] = ???
    }
    
    context.add(new TestConstraint)
    val values1 = Array(1, 7, 6, 8, 3)
    val values2 = Array(1, 7, 6)
    val values3 = Array(1, 7)
    val values4 = Array(1)
    variable.restrict(values1, values1.length)
    context.propagate()
    assert(n == 1)
    variable.restrict(values2, values2.length)
    context.propagate()
    assert(n == 2)
    variable.restrict(values3, values3.length)
    context.propagate()
    assert(n == 2)
    variable.restrict(values4, values4.length)
    context.propagate()
    assert(n == 3)
  }

  test("minBy should be stable: always return the same value for the same function and store.") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 0, 2, true)
    val f = Array(1, 1, 2)

    val minbyBefore = variable.minBy(f)
    assert(minbyBefore != 2)

    store.pushState()
    variable.removeValue(1) // remove another value that also has the minby value
    assert(variable.minBy(f) == minbyBefore)
    store.pop() // restore the store

    val minbyAfter = variable.minBy(f)
    assert(minbyBefore == minbyAfter)
  }

  test("maxBy should be stable: always return the same value for the same function and store.") {
    val store = new CPStore()
    val variable = new CPIntVarAdaptable(store, 0, 3, true)
    val f = Array(1, 1, 1, 0)

    val maxbyBefore = variable.maxBy(f)
    assert(maxbyBefore != 3)

    store.pushState()
    variable.removeValue(1) // remove another value that also has the maxby value
    assert(variable.maxBy(f) == maxbyBefore)
    store.pop() // restore the store

    val maxbyAfter = variable.maxBy(f)
    assert(maxbyBefore == maxbyAfter)
  }
  
}
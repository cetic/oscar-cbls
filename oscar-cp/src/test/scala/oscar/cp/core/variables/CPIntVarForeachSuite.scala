package oscar.cp.core.variables

import java.util.ConcurrentModificationException

import oscar.cp._
import oscar.cp.core.{CPPropagStrength, CPStore, Constraint}
import oscar.cp.isInconsistent
import oscar.cp.testUtils._

import scala.collection.mutable
import scala.util.Random

class CPIntVarForeachSuite extends TestSuite {

  test("Should only iterate on values present inside the domain - continuous") {
    val store = new CPStore()
    val r = new Random(793190)
    for(test <- 0 until 100) {
      val v = CPIntVar(0, 100000)(store)
      assert(v.isInstanceOf[CPIntVarAdaptable] && v.isContinuous)

      v.updateMin(r.nextInt(50000))
      v.updateMax(r.nextInt(50000) + 50000)

      for(i <- v) {
        assert(v.min <= i && i <= v.max)
      }
    }
  }

  test("Should only iterate on values present inside the domain - sparse") {
    val store = new CPStore()
    val r = new Random(793190)
    for(test <- 0 until 100) {
      val v = CPIntVar(100, 100000)(store)
      assert(v.isInstanceOf[CPIntVarAdaptable] && v.isContinuous)

      val present = new mutable.HashSet[Int]()

      for(i <- 100 to 100000) {
        if(r.nextDouble() > 0.6) {
          present.add(i)
        }
        else
          v.removeValue(i)
      }

      assert(!v.isContinuous && v.asInstanceOf[CPIntVarAdaptable].offset != 0)

      val seen = new mutable.HashSet[Int]()

      for(i <- v) {
        assert(present.contains(i))
        assert(!seen.contains(i))
        seen.add(i)
      }

      assert(seen.size == present.size)
    }
  }

  test("Delete - sparse") {
    val store = new CPStore()
    val r = new Random(793190)
    for(test <- 0 until 100) {
      val v = CPIntVar(100, 100000)(store)
      assert(v.isInstanceOf[CPIntVarAdaptable] && v.isContinuous)

      v.removeValue(1000)
      assert(!v.isContinuous)

      // reorder things inside the reversible set
      for(_ <- 0 until 10) {
        store.pushState()
        for(i <- 100 to 100000) {
          if (r.nextDouble() > 0.6) {
            v.removeValue(i)
          }
        }
        store.pop()
      }

      val present = new mutable.HashSet[Int]()

      for(i <- 100 to 100000) {
        if(v.hasValue(i) && r.nextDouble() > 0.6) {
          present.add(i)
        }
        else
          v.removeValue(i)
      }

      assert(!v.isContinuous && v.asInstanceOf[CPIntVarAdaptable].offset != 0)

      val seen = new mutable.HashSet[Int]()

      for(i <- v) {
        assert(present.contains(i))
        assert(!seen.contains(i))
        seen.add(i)
        if(r.nextDouble() < 0.5)
          v.removeValue(i)
      }

      assert(seen.size == present.size)

    }
  }

  test("Delete - continuous to sparse") {
    val store = new CPStore()
    val r = new Random(793190)
    for(test <- 0 until 10) {
      val v = CPIntVar(100, 100000)(store)
      assert(v.isInstanceOf[CPIntVarAdaptable] && v.isContinuous)

      val seen = new mutable.HashSet[Int]()

      // Attempt to delete contiguously
      for(i <- v) {
        assert(100 <= i && i <= 100000)
        assert(!seen.contains(i))
        seen.add(i)
        if(i < 200) {
          v.removeValue(i)
        }
      }

      assert(v.isContinuous)
      assert(seen.size == 100000-100+1)
      seen.clear()
      // Then delete the remaining parts
      for(i <- v) {
        assert(200 <= i && i <= 100000)
        assert(!seen.contains(i))
        seen.add(i)
        if(r.nextDouble() < 0.5)
          v.removeValue(i)
      }

      assert(seen.size == 100000-200+1)
    }
  }

  test("Delete - continuous - delete outside") {
    val store = new CPStore()
    val r = new Random(793190)
    for(test <- 0 until 100) {
      val v = CPIntVar(100, 100000)(store)
      assert(v.isInstanceOf[CPIntVarAdaptable] && v.isContinuous)

      val breakAt = r.nextInt(50000)+25000
      var removeAt = r.nextInt(50000)+25000
      if(removeAt == breakAt)
        removeAt += 1
      try {
        for (i <- v) {
          if (i == breakAt)
            v.removeValue(removeAt)
        }
        assert(false)
      }
      catch {
        case _: ConcurrentModificationException =>
      }
    }
  }

  test("Delete - sparse - delete outside") {
    val store = new CPStore()
    val r = new Random(793190)
    for(test <- 0 until 100) {
      val v = CPIntVar(100, 100000)(store)
      assert(v.isInstanceOf[CPIntVarAdaptable] && v.isContinuous)

      v.removeValue(45000)
      assert(!v.isContinuous)

      val breakAt = r.nextInt(50000)+25000
      var removeAt = r.nextInt(50000)+25000
      if(removeAt == breakAt)
        removeAt += 1
      try {
        for (i <- v) {
          if (i >= breakAt)
            v.removeValue(removeAt)
        }
        assert(false)
      }
      catch {
        case _: ConcurrentModificationException =>
      }
    }
  }

  test("CPBoolvar") {
    val store = new CPStore()
    var v = CPBoolVar()(store)

    var seenTrue = false
    var seenFalse = false

    for(i <- v) {
      if(i == 0) {
        assert(!seenFalse)
        seenFalse = true
      }
      else if(i == 1) {
        assert(!seenTrue)
        seenTrue = true
      }
      else
        assert(false)
    }

    assert(seenFalse && seenTrue)

    v = CPBoolVar()(store)
    v.assignTrue()
    seenTrue = false
    seenFalse = false
    for(i <- v) {
      if(i == 0) {
        assert(!seenFalse)
        seenFalse = true
      }
      else if(i == 1) {
        assert(!seenTrue)
        seenTrue = true
      }
      else
        assert(false)
    }
    assert(!seenFalse && seenTrue)

    v = CPBoolVar()(store)
    v.assignFalse()
    seenTrue = false
    seenFalse = false
    for(i <- v) {
      if(i == 0) {
        assert(!seenFalse)
        seenFalse = true
      }
      else if(i == 1) {
        assert(!seenTrue)
        seenTrue = true
      }
      else
        assert(false)
    }
    assert(seenFalse && !seenTrue)
  }

  test("CPBoolvar - delete") {
    val store = new CPStore()
    val v = CPBoolVar()(store)

    var seenTrue = false
    var seenFalse = false

    for(i <- v) {
      if(i == 0) {
        assert(!seenFalse)
        seenFalse = true
      }
      else if(i == 1) {
        assert(!seenTrue)
        seenTrue = true
      }
      else
        assert(false)

      if(i == 0) {
        v.removeValue(0)
      }
    }
    assert(seenFalse && seenTrue)
  }

  test("CPBoolvar - delete - outside") {
    val store = new CPStore()
    val v = CPBoolVar()(store)

    try {
      for (i <- v) {
        if (i == 0) {
          v.removeValue(1)
        }
      }
      assert(false)
    }
    catch {
      case _: ConcurrentModificationException =>
    }
  }

  test("Singleton") {
    val store = new CPStore()
    val v = CPIntVar(8)(store)

    var seen = false
    for(i <- v) {
      assert(i == 8)
      assert(!seen)
      seen = true
    }
  }

  test("Linear things") {
    val store = new CPStore()
    val v = CPIntVar(1000, 200000)(store)
    val v2: CPIntVar = v * 2
    val v2plus2: CPIntVar = v2 + 2

    val vm2minusm2 = -v2plus2

    val p = new mutable.HashSet[Int]()
    for(i <- 1000 to 200000)
      p.add(-2*i-2)

    for(i <- vm2minusm2) {
      assert(p.contains(i))
      p.remove(i)
    }

    assert(p.isEmpty)
  }

  test("Bug 1") {
    val solver = CPSolver()
    val v = CPIntVar(0 to 1)(solver)
    v.foreach { p =>
      if (p == 1)
        v.removeValue(p)
    }
  }
}
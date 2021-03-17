/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.cp.test

import oscar.cp._
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.variables.CPVar
import oscar.cp.testUtils._

import scala.collection.mutable.ArrayBuffer


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestDeltaPropagate extends TestSuite {
  
  test("test delta 1") {
    var propag = false
    
    class MyCons(val X: CPIntVar) extends Constraint(X.store, "TestDelta") {
      
      var snapshot: DeltaIntVar = null

      override def setup(l: CPPropagStrength): Unit = {
        snapshot = X.callPropagateOnChangesWithDelta(this)
      }
      
     override def propagate(): Unit = {
        //println(X.delta(this).toSet)
        
        snapshot.changed should be(true)
        snapshot.size should be(2)
        snapshot.values.toSet should be(Set(5,7))
        snapshot.maxChanged should be(true)
        snapshot.minChanged should be(false)
        snapshot.oldMin should be(1)
        snapshot.oldMax should be(7)
        
        
        propag = true
      }

      override def associatedVars(): Iterable[CPVar] = ???
    }

    val cp = CPSolver()
    val x = CPIntVar(Array(1, 3, 5, 7))(cp)
    cp.add(new MyCons(x))
    cp.add(x < 5)
    propag should be(true)
  }
  


  test("test delta 2") {
    var propag = false
    
    class MyCons(val X: CPIntVar) extends Constraint(X.store, "TestDelta") {

      var snapshot: DeltaIntVar = null

      override def setup(l: CPPropagStrength): Unit = {
        snapshot = X.callPropagateOnChangesWithDelta(this)
      }
      
      override def propagate(): Unit = {
        //println(X.delta(this).toSet)
        
        snapshot.changed should be(true)
        snapshot.size should be(2)
        snapshot.values.toSet should be(Set(2,4))
        snapshot.maxChanged should be(true)
        snapshot.minChanged should be(false)
        snapshot.oldMin should be(-2)
        snapshot.oldMax should be(4)
        
        
        propag = true
      }

      override def associatedVars(): Iterable[CPVar] = ???
    }

    val cp = CPSolver()
    val x = CPIntVar(Array(1, 3, 5, 7))(cp) -2 -3 + 2 // -2,0,2,4
    cp.add(new MyCons(x))
    cp.add(x < 2)
    propag should be(true)
  }
  
  test("test delta 3") {
    var propag = false
    
    class MyCons(val X: CPIntVar) extends Constraint(X.store, "TestDelta") {
      priorityL2 = CPStore.MaxPriorityL2-5
      var snapshot: DeltaIntVar = null

      override def setup(l: CPPropagStrength): Unit = {
        snapshot = X.callPropagateOnChangesWithDelta(this)
      }
      
      override def propagate(): Unit = {
        //println(X.delta(this).toSet)
        
        snapshot.changed should be(true)
        snapshot.size should be(2)
        snapshot.values.toSet should be(Set(2,4))
        snapshot.maxChanged should be(true)
        snapshot.minChanged should be(false)
        snapshot.oldMin should be(-2)
        snapshot.oldMax should be(4)
        
        
        propag = true
      }
      override def associatedVars(): Iterable[CPVar] = ???
    }

    val cp = CPSolver()
    val x = CPIntVar(Array(1, 3, 5, 7))(cp) -2 -3 + 2 // -2,0,2,4
    cp.add(new MyCons(x))
    val cons = ArrayBuffer[Constraint]()
    cons.append(x < 4)
    cons.append(x < 2)
    cp.add(cons)
    //println("x dom:"+x.toSet)
    propag should be(true)
  }    
  



  test("test delta 4") {
    var propag = false
    
    class MyCons(val X: CPIntVar) extends Constraint(X.store, "TestDelta") {
      priorityL2 = CPStore.MaxPriorityL2-5 
      override def setup(l: CPPropagStrength): Unit = {
        X.filterWhenDomainChangesWithDelta() { delta =>
          propag = true
          delta.changed should be(true)
          delta.size should be(2)
          delta.values.toSet should be(Set(2,4))
          delta.maxChanged should be(true)
          delta.minChanged should be(false)
          delta.oldMin should be(-2)
          delta.oldMax should be(4)
          false
        }
      }
      override def associatedVars(): Iterable[CPVar] = ???
    }

    val cp = CPSolver()
    val x = CPIntVar(Array(1, 3, 5, 7))(cp) -2 -3 + 2 // -2,0,2,4
    cp.add(new MyCons(x))
    val cons = ArrayBuffer[Constraint]()
    cons.append(x < 4)
    cons.append(x < 2)
    cp.add(cons)
    // println("x dom:"+x.toSet)
    propag should be(true)
  }
  
  
  test("test delta 5 (with views)") {
    var propag = false
    
    class MyCons(val X: CPIntVar) extends Constraint(X.store, "TestDelta") {
      priorityL2 = CPStore.MaxPriorityL2-5 
      override def setup(l: CPPropagStrength): Unit = {
        X.filterWhenDomainChangesWithDelta() { delta =>
          propag = true
          delta.changed should be(true)
          delta.size should be(2)
          delta.values.toSet should be(Set(-2,-4))
          delta.maxChanged should be(false)
          delta.minChanged should be(true)
          delta.oldMin should be(-4)
          delta.oldMax should be(2)
          false
        }
      }

      override def associatedVars(): Iterable[CPVar] = ???
    }

    val cp = CPSolver()
    val x = -(CPIntVar(Array(1, 3, 5, 7))(cp) -2 -3 + 2) // -4,-2,0,2
    cp.add(new MyCons(x))
    val cons = ArrayBuffer[Constraint]()
    cons.append(x > -4)
    cons.append(x > -2)
    cp.add(cons)
    //println("x dom:"+x.toSet)
    propag should be(true)
  }

  test("test delta 6: queens") {

    class Cons1(val x: Array[CPIntVar]) extends Constraint(x(0).store, "Cons1") {

      val delta1 = Array.tabulate(x.size)(i => Array.ofDim[Int](x(i).size))
      val delta2 = Array.tabulate(x.size)(i => Array.ofDim[Int](x(i).size))
      var currDim = Array.tabulate(x.size)(i => 0)
      
      val snapshots = new Array[DeltaIntVar](x.length)

      override def setup(l: CPPropagStrength): Unit = {

        for (i <- 0 until x.size) {
          snapshots(i) = x(i).callPropagateOnChangesWithDelta(this)
          x(i).callValRemoveIdxWhenValueIsRemoved(this, i)
        }
        propagate()

      }

      s.onPop {
        for (i <- 0 until x.size) {
          currDim(i) = 0
        }
      }

      override def valRemoveIdx(x: CPIntVar, idx: Int, value: Int) = {
        delta1(idx)(currDim(idx)) = value
        currDim(idx) += 1
      }
     
      override def propagate(): Unit = {
        for (i <- 0 until x.size) {

          val m = snapshots(i).fillArray(delta2(i))
          val s1 = delta2(i).take(m)
          val s2 = delta1(i).take(currDim(i))
          val s3 = snapshots(i).values.toArray
          assert(m == currDim(i))
          
          assert(s1.length == s2.length)
          assert(s1.length == s3.length)
          assert(s1.toSet == s2.toSet)
          assert(s1.toSet == s3.toSet)          
          
        }

        for (i <- 0 until x.size) {
          currDim(i) = 0
        }
      }

      override def associatedVars(): Iterable[CPVar] = ???
    }

    implicit val cp = CPSolver()
    cp.silent = true
    val n = 11 //number of queens
    val Queens = 0 until n
    //variables
    val queens = for (i <- Queens) yield CPIntVar(1 to n)(cp)

    var nbsol = 0

    cp.add(new Cons1(queens))
    cp.add(new Cons1(for (i <- Queens) yield queens(i) + i))
    cp.add(new Cons1(for (i <- Queens) yield queens(i) - i))

    cp.add(allDifferent(queens), Strong)
    cp.add(allDifferent(for (i <- Queens) yield queens(i) + i), Strong)
    cp.add(allDifferent(for (i <- Queens) yield queens(i) - i), Strong)

    // Search heuristic
    search(binaryFirstFail(queens,_.randomValue))

    // Execution
    val stats = start()
    //println(stats)

  }
  
  
  test("test delta 7") {
    var nPropagates = 0
    
    class MyCons(val X: CPIntVar) extends Constraint(X.store, "TestDelta") {
      priorityL2 = CPStore.MaxPriorityL2-5
      
      var snapshot: DeltaIntVar = null
      
      override def setup(l: CPPropagStrength): Unit = {
        snapshot = X.callPropagateOnChangesWithDelta(this)
        // I remove some values such that propagate should be called again
        X.updateMax(5)
        X.updateMin(1)
        X.removeValue(3)
      }
      override def propagate(): Unit = {
        nPropagates += 1
        assert(snapshot.values.toSet == Set(0,3,6,7,8)) 
        val array = Array.ofDim[Int](6)
        val m = snapshot.fillArray(array)
        assert(m == 5)
        assert(array.take(m).toSet == Set(0,3,6,7,8))
      }

      override def associatedVars(): Iterable[CPVar] = ???
    }
    
  

    val cp = CPSolver()
    val x = (CPIntVar(0 to 8)(cp) -2 + 2)
    cp.add(new MyCons(x))

    //println("x dom:"+x.toSet)
    assert(nPropagates == 1)
  }  
 
  
  test("test delta 8 (with views)") {
    var nPropag = 0

    class MyCons(val X: CPIntVar) extends Constraint(X.store, "TestDelta") {
      priorityL2 = CPStore.MaxPriorityL2 - 5
      override def setup(l: CPPropagStrength): Unit = {
        X.filterWhenDomainChangesWithDelta() { delta =>
          nPropag += 1

          if (nPropag == 1) {
            delta.changed should be(true)
            delta.size should be(2)
            val d = delta.values.toArray
            d.size should be(2)
            d.toSet should be(Set(-2, -4))
            delta.maxChanged should be(false)
            delta.minChanged should be(true)
            delta.oldMin should be(-4)
            delta.oldMax should be(2)
          }
          if (nPropag == 2) {
            delta.changed should be(true)
            delta.size should be(1)
            delta.values.toArray should be(Array(0))
            delta.maxChanged should be(false)
            delta.minChanged should be(true)
            delta.oldMin should be(0)
            delta.oldMax should be(2)
          }
          false
        }
      }
      override def associatedVars(): Iterable[CPVar] = ???
    }

    val cp = CPSolver()
    val x = -(CPIntVar(Array(1, 3, 5, 7))(cp) -2 -3 + 2) // -4,-2,0,2
    cp.add(new MyCons(x))
    
    val cons = ArrayBuffer[Constraint]()
    cons.append(x > -4)
    cons.append(x > -2)
    cp.add(cons)
    
    cp.add(x > 0)
    
    //println("x dom:"+x.toSet)
    nPropag should be(2)
  }
   
  test("test delta 9 (with views) idempotent") {
    var nPropag = 0

    class MyCons(val X: CPIntVar) extends Constraint(X.store, "TestDelta") {
      priorityL2 = CPStore.MaxPriorityL2 - 5
      override def setup(l: CPPropagStrength): Unit = {
        X.filterWhenDomainChangesWithDelta(idempotent = true) { delta =>
          nPropag += 1

          if (nPropag == 1) {
            delta.changed should be(true)
            delta.size should be(2)
            val d = delta.values.toArray
            d.size should be(2)
            d.toSet should be(Set(-2, -4))
            delta.maxChanged should be(false)
            delta.minChanged should be(true)
            delta.oldMin should be(-4)
            delta.oldMax should be(2)
            X.updateMin(1)
  
          }
          if (nPropag == 2) {
            // should not come here since it is idempotent, don't know what should be the correct delta in this case
            //println("oldMin:"+delta.oldMin+" oldMax:"+delta.oldMax+" size:"+delta.size)
            delta.changed should be(true)
            delta.size should be(1)
            delta.values.toArray should be(Array(0))
            delta.maxChanged should be(false)
            delta.minChanged should be(true)
            delta.oldMin should be(0)
            delta.oldMax should be(2)
          }
          false
        }
      }
      override def associatedVars(): Iterable[CPVar] = ???
    }

    val cp = CPSolver()
    val x = CPIntVar(Array(-4,-2,0,2))(cp) // -4,-2,0,2  //-(CPIntVar(Array(1, 3, 5, 7))(cp) -2 -3 + 2) // -4,-2,0,2
    cp.add(new MyCons(x))
    
    val cons = ArrayBuffer[Constraint]()
    cons.append(x.gr(-4))
    cons.append(x.gr(-2))
    cp.add(cons)
    
    //println("x dom:"+x.toSet)
    nPropag should be(1)
  }


  test("test delta 10 filterNotIdempotent not idempotent") {
    var nPropag = 0

    class MyCons(val X: CPIntVar) extends Constraint(X.store, "TestDelta") {
      priorityL2 = CPStore.MaxPriorityL2 - 5
      override def setup(l: CPPropagStrength): Unit = {
        X.filterWhenDomainChangesWithDelta(idempotent = false) { delta =>
          nPropag += 1

          X.updateMin(-1)

          false
        }
      }
      override def associatedVars(): Iterable[CPVar] = ???
    }

    val cp = CPSolver()
    val x = CPIntVar(Array(-4,-2,0,2))(cp) // -4,-2,0,2  //-(CPIntVar(Array(1, 3, 5, 7))(cp) -2 -3 + 2) // -4,-2,0,2
    cp.add(new MyCons(x))


    cp.add(x > -4)
    println(x)
    assert(x.min == 0)

    //println("x dom:"+x.toSet)
    nPropag should be(2)
  }

  test("test delta 11 filterNotIdempotent not idempotent") {
    var nPropag = 0


    class MyCons(val X: CPIntVar) extends Constraint(X.store, "TestDelta") {
      priorityL2 = CPStore.MaxPriorityL2 - 5
      override def setup(l: CPPropagStrength): Unit = {
        X.callOnChanges(delta => {
          if (nPropag == 0) {
            assert(delta.size == 1)
          }
          if (nPropag == 1) {
            assert(delta.size == 0)
          }
          nPropag += 1
          X.updateMin(-1)
          false
        },idempotent=false)
      }
      override def associatedVars(): Iterable[CPVar] = ???
    }

    val cp = CPSolver()
    val x = CPIntVar(Array(-4,-2,0,2))(cp) // -4,-2,0,2  //-(CPIntVar(Array(1, 3, 5, 7))(cp) -2 -3 + 2) // -4,-2,0,2
    cp.add(new MyCons(x))

    cp.add(x > -4)
    nPropag should be(2)
    assert(x.min == 0)

    //println("x dom:"+x.toSet)
    nPropag should be(2)
  }


}

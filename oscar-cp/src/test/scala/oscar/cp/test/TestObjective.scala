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

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._

class TestObjective extends FunSuite with ShouldMatchers {

  test("Obj1") {
    val cp = CPSolver()
    val x1 = CPVarInt(1 to 3)(cp)
    val x2 = CPVarInt(1 to 3)(cp)
    cp.add(x1 + x2 == 4)

    val obj1 = new CPObjectiveUnitMinimize(x1, "x1")
    obj1.tightenMode = TightenType.NoTighten
    val obj2 = new CPObjectiveUnitMinimize(x2, "x2")
    obj2.tightenMode = TightenType.NoTighten


    val obj = new CPObjective(cp, obj1, obj2)
    cp.optimize(obj) 
    cp.search {
      binary(Array(x1, x2), minVar, _.max)
    } 
    cp.start().nSols should be(3)
  }

  test("Obj2") {
    val cp = CPSolver()
    val x1 = CPVarInt(2 to 3)(cp)
    val x2 = CPVarInt(1 to 3)(cp)

    val obj1 = new CPObjectiveUnitMinimize(x1, "x1")
    obj1.tightenMode = TightenType.WeakTighten
    val obj2 = new CPObjectiveUnitMinimize(x2, "x2")
    obj2.tightenMode = TightenType.WeakTighten


    val obj = new CPObjective(cp, obj1, obj2)
    cp.optimize(obj) 
    cp.search {
      binaryStatic(Array(x1, x2), _.max)
    } onSolution {
      println("=>"+x1+" "+x2)
    }
    cp.start().nSols should be(4)
  }

  test("Obj3") {
    val cp = CPSolver()
    val x1 = CPVarInt(2 to 3)(cp)
    val x2 = CPVarInt(1 to 3)(cp)

    val obj1 = new CPObjectiveUnitMinimize(x1, "x1")
    obj1.tightenMode = TightenType.StrongTighten
    val obj2 = new CPObjectiveUnitMinimize(x2, "x2")
    obj2.tightenMode = TightenType.WeakTighten


    val obj = new CPObjective(cp, obj1, obj2)
       
    cp.optimize(obj) 
    cp.search {
      binary(Array(x1, x2), minVar, _.max)
    } onSolution {
       // solutions are (3,3) (2,3)
      Set((3, 3), (2, 3)).contains((x1.value, x2.value)) should be(true)
    } 
    cp.start().nSols should be(2)
  }

  test("Obj4") {
    val cp = CPSolver()
    val x1 = CPVarInt(2 to 3)(cp)
    val x2 = CPVarInt(1 to 3)(cp)

    val obj1 = new CPObjectiveUnitMinimize(x1, "x1")
    obj1.tightenMode = TightenType.StrongTighten
    val obj2 = new CPObjectiveUnitMinimize(x2, "x2")
    obj2.tightenMode = TightenType.StrongTighten


    val obj = new CPObjective(cp, obj1, obj2)
    cp.optimize(obj) 
    cp.search {
      binary(Array(x1, x2), minVar, _.max)
    } onSolution {
      // solutions are (3,3) (2,2)
      Set((3, 3), (2, 2)).contains((x1.value, x2.value)) should be(true)
    } 
    cp.start().nSols should be(2)
  }

  test("Obj5") {

    val cp = CPSolver()
    val x1 = CPVarInt(0 to 2)(cp)
    val x2 = CPVarInt(0 to 2)(cp)

    val obj1 = new CPObjectiveUnitMinimize(x1, "x1")
    obj1.tightenMode = TightenType.NoTighten
    val obj2 = new CPObjectiveUnitMinimize(x2, "x2")
    obj2.tightenMode = TightenType.NoTighten
    val obj3 = new CPObjectiveUnitMinimize((x1 * 2) + x2, "2*x1+x2")
    obj3.tightenMode = TightenType.StrongTighten

    val obj = new CPObjective(cp, obj1, obj2, obj3)    
    cp.optimize(obj) 
    cp.search {
      binary(Seq(x1), minVar, _.max) ++ binary(Seq(x2), minVar, _.max)
    }
    cp.start().nSols should be(7)    
    
  }

  test("Obj6") {
    val cp = CPSolver()
    val x1 = CPVarInt(0 to 2)(cp)
    val x2 = CPVarInt(0 to 2)(cp)

    val obj1 = new CPObjectiveUnitMinimize(x1, "x1")
    obj1.tightenMode = TightenType.WeakTighten
    val obj2 = new CPObjectiveUnitMinimize(x2, "x2")
    obj2.tightenMode = TightenType.WeakTighten
    val obj3 = new CPObjectiveUnitMinimize((x1 * 2) + x2, "2*x1+x2")
    obj3.tightenMode = TightenType.StrongTighten

    var nbsol = 0

    val obj = new CPObjective(cp, obj1, obj2, obj3)    
    cp.optimize(obj) 
    cp.search {
      binary(Seq(x1), minVar, _.max) ++ binary(Seq(x2), minVar, _.max)
    }
    cp.start().nSols should be(5) 
  }

  test("Obj7") {
    val cp = CPSolver()
    val x1 = CPVarInt(0 to 2)(cp)
    val x2 = CPVarInt(0 to 2)(cp)

    val obj1 = new CPObjectiveUnitMinimize(x1, "x1")
    obj1.tightenMode = TightenType.StrongTighten
    val obj2 = new CPObjectiveUnitMinimize(x2, "x2")
    obj2.tightenMode = TightenType.WeakTighten
    val obj3 = new CPObjectiveUnitMinimize((x1 * 2) + x2, "2*x1+x2")
    obj3.tightenMode = TightenType.StrongTighten

    var nbsol = 0

    val obj = new CPObjective(cp, obj1, obj2, obj3)
    cp.optimize(obj) 
    cp.search {
      binary(Seq(x1), minVar, _.max) ++ binary(Seq(x2), minVar, _.max)
    }
    cp.start().nSols should be(3) 
  }

  test("Obj8") {

    val cp = new CPSolver();
    val x = CPVarInt(Array(1, 5, 9, 10))(cp)
    var nbSol = 0
    cp.minimize(x)
    cp.search {
      binaryFirstFail(Array(x), valHeuris = (x: CPVarInt) => x.max)
    }
    cp.start().nSols should be(4)
  }   
 

}
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
package oscar.cbls.test.invariants

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.core.computation.{CBLSIntVar, Domain, Store}

import scala.language.postfixOps

class TestIntVar extends AnyFunSuite with Matchers {
  
  test("test create IntVar 1..10"){
    val solver = new Store
    val x = CBLSIntVar(solver, 1, 1 to 10, "x")
    val y = CBLSIntVar(solver, 1, 1 to 10, "y")
    val z = CBLSIntVar(solver, 1, 1 until 10, "z")
  //  solver.close()
    
    x.min should be(1)
    x.max should be(10)
    
    y.min should be(1)
    y.max should be(10)
    
    z.min should be(1)
    z.max should be(9)
  }
  
  test("test create IntVar using invalid ranges"){
	  val solver = new Store
    val x = CBLSIntVar(solver, 1, 0 to -1, "x")
    val y = CBLSIntVar(solver, 0, 0 until 0, "y")
    // Behaviour changed: not valid ranges become
    // empty domains

    x.domain should be(Domain.empty)
    y.domain should be(Domain.empty)

    // In case of behaviour changes again, I let the old tests
    /*
    an [NoSuchElementException] should be thrownBy {
      val x = CBLSIntVar(solver, 1, 0 to -1, "x")
    }

    an [NoSuchElementException] should be thrownBy {
      val x = CBLSIntVar(solver, 0, 0 until 0, "x") // until makes an empty range
    }
     */
  }
  
  test("test inDomain of IntVar 1..10"){
    val solver = new Store
    val x = CBLSIntVar(solver, 1, 1 to 10, "x")
    solver.close()
    (1 to 10).foreach(x.domain.contains(_) should be(true))
    x.domain.contains(0) should be(false)
    x.domain.contains(11) should be(false)
    x.domain.contains(Int.MaxValue) should be(false)
    x.domain.contains(Int.MinValue) should be(false)
  }
  
  test("test domain of IntVar"){
    val solver = new Store
    val domain:Domain = 1 to 10
    val x = CBLSIntVar(solver, 1, domain, "x")
    val y = CBLSIntVar(solver, 1, 1 to 10, "y")
    
    x.domain should be(domain)
    y.domain should be(domain)
  }
  
  test("test setValue via :="){
    val solver = new Store
    
    val x = CBLSIntVar(solver, 1, 1 to 10, "x")
    solver.close()
    
    x.value should be(1)
    x := 2
    x.value should be(2)
  }
  
  test("test :=:"){
    val solver = new Store
    val x = CBLSIntVar(solver, 1, 1 to 10, "x")
    val y = CBLSIntVar(solver, 10, 1 to 10, "y")
    solver.close()
    
    x.value should be(1)
    y.value should be(10)
    
    x :=: y
    
    x.value should be(10)
    y.value should be(1)
  }
  
  test("test :+=") {
    val solver = new Store
    val x = CBLSIntVar(solver, 50, 1 to 100, "x")
    
    x.value should be(50)
    x :+= 10
    x.value should be(60)
    x :+= -5
    x.value should be(55)
  }
  
  test("test :*=") {
    val solver = new Store
    val x = CBLSIntVar(solver, 10, 1 to 30, "x")
    
    x.value should be(10)
    x :*= 2
    x.value should be(20)
    x :*= 1
    x.value should be(20)
  }
  
  test("test :-=") {
    val solver = new Store
    val x = CBLSIntVar(solver, 5, 1 to 10, "x")
    x.value should be(5)
    x :-= 3
    x.value should be(2)
    x :-= -4 
    x.value should be(6)
    x :-= 0
    x.value should be(6)
  }
  
  test("test ++()") {
    val solver = new Store
    val x = CBLSIntVar(solver, 1, 1 to 10, "x")
    x.value should be(1)

    x ++

    x.value should be(2)

    x ++

    x.value should be(3)
  }
}

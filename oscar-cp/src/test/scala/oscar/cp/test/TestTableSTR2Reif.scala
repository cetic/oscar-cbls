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
import oscar.cp.testUtils.TestSuite
import oscar.cp.constraints._
import oscar.cp.constraints.tables.TableSTR2Reif
import oscar.cp._
import org.junit.runner.RunWith

class TestTableSTR2Reif extends TestSuite  {
  test("Table Test 1") {
    /*
     * x0 = x1 = x2 = {1}
     * table
     * 0 1 2
     * -----
     * 1 1 1
     * 
     * b is true
     * 
     */
    
    val cp = CPSolver()
    var x = Array.fill(3)(CPIntVar(1 to 1)(cp))
    var b = CPBoolVar()(cp)
    val tuples = Array(Array(1,1,1))
    
    cp.post(new TableSTR2Reif(x,tuples,b))
    //b.isBound should be(true)
    b.isTrue should be(true)

  }
  
  test("Table Test 2") {
    /*
     * x0 = x1 = {0, 1} x2 = {0}
     * 0 1 2
     * -----
     * 0 0 0
     * 0 1 0
     * 1 0 0
     * 1 1 1
     * 
     * b is true
     */
    
    val cp = CPSolver()
    var x = CPIntVar(0 to 1)(cp)
    var y = CPIntVar(0 to 1)(cp)
    var z = CPIntVar(0 to 0)(cp)
    var b = CPBoolVar()(cp)
    val tuples = Array(Array(0,0,0),Array(0,1,0),Array(1,0,0),Array(1,1,0)) 
    
    cp.post(new TableSTR2Reif(Array(x,y,z),tuples,b))
    b.isBound should be(true)
    b.isTrue should be(true)
  }
  
  test("Table Test 3") {
    /*
     * x0 = x1 = x2 = {3,4,5}
     * 0 1 2
     * -----
     * 1 1 1
     * 1 2 1
     * 1 2 2
     * 2 1 1
     * 
     * b is false (table become empty)
     * 
     */

    val cp = CPSolver()
    var x = Array.fill(3)(CPIntVar(3 to 5)(cp))
    var b = CPBoolVar()(cp)
    val tuples = Array(Array(1,1,1),Array(1,2,1),Array(1,2,2),Array(2,1,1))
    
    cp.post(new TableSTR2Reif(x,tuples,b))
    b.isFalse should be(true)
    
  }
  
  test("Table Test 4") {
    /*
     * x0 = x1 = x2 = {3,4}
     * 0 1 2
     * -----
     * 1 1 1
     * 1 2 1
     * 1 2 2
     * 2 1 1
     * 3 3 3
     * 4 3 3
     * 
     * b is not bound (dom(b) = {true, false})
     * 
     * But after assignment x1 = 3 , x2 = 3
     * b must be true
     * 
     */

    val cp = CPSolver()
    var x = Array.fill(3)(CPIntVar(3 to 4)(cp))
    var b = CPBoolVar()(cp)
    val tuples = Array(Array(1,1,1),Array(1,2,1),Array(1,2,2),Array(2,1,1),Array(3,3,3),Array(4,3,3))
    
    cp.post(new TableSTR2Reif(x,tuples,b))
    b.isBound should be(false)

    cp.post(x(1) === 3)
    cp.post(x(2) === 3)
    b.isTrue should be(true)

  }
    
  test("Table Test 5") {
    /*
     * x0 = x1 = x2 = {3,4}
     * 0 1 2
     * -----
     * 1 1 1
     * 1 2 1
     * 1 2 2
     * 2 1 1
     * 2 3 3
     * 3 3 3
     * 
     * b is not bound (dom(b) = {true, false})
     * 
     * But after assignment x1 = 2 , x2 = 3
     * b must be false because table will be empty
     * 
     */

    val cp = CPSolver()
    var x = Array.fill(3)(CPIntVar(1 to 3)(cp))
    var b = CPBoolVar()(cp)
    val tuples = Array(Array(1,1,1),Array(1,2,1),Array(1,2,2),Array(2,1,1),Array(2,3,3),Array(3,3,3))
    
    cp.post(new TableSTR2Reif(x,tuples,b))
    b.isBound should be(false)
    
    cp.post(x(1) === 2)
    cp.post(x(2) === 3)
    b.isFalse should be(true)
    
  }
    
  test("Table Test 6") {
    /*
     * x0 = x1 = x2 = {1,2,3}
     * 0 1 2
     * -----
     * 1 1 1
     * 1 1 2
     * 1 1 3
     * 1 2 1
     * 1 2 2
     * 1 2 3
     * ...
     * 3 3 3
     * (27 tuples)
     * 
     * b must be true 
     * 
     */

    val cp = CPSolver()
    var x = Array.fill(3)(CPIntVar(1 to 3)(cp))
    var b = CPBoolVar()(cp)
    val tuples = (for (i <- 1 to 3; j <- 1 to 3; k <- 1 to 3) yield Array(i,j,k)).toArray
    
    cp.post(new TableSTR2Reif(x,tuples,b))
    b.isTrue should be(true)
  }
    
  test("Table Test 7") {    
    val cp = CPSolver()
    var x = Array.fill(2)(CPIntVar(5 to 7)(cp))
    var b = CPBoolVar()(cp)
    val tuples = (for (i <- 1 to 3; j <- 1 to 3) yield Array(i,j)).toArray
    
    cp.post(new TableSTR2Reif(x,tuples,b))
    b.isBound should be(true)
    b.isFalse should be(true)
  }
    
}

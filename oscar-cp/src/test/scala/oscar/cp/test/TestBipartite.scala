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
import oscar.cp.constraints._
import oscar.cp.core.variables.CPGraphVar
import oscar.cp.testUtils.TestSuite

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 */

class TestBipartite extends TestSuite {
  
  test("Test 1 : Test constraint initial propagation") {
    val cp = CPSolver()
    val nnodes : Int = 3
    val g = CPGraphVar(nnodes)(cp)
    // g.edges are (0,1),(0,2),(1,0),(1,2),(2,0),(2,1)
    
    // 1) add some mandatory nodes/edges
    postAndCheckSuspend(cp, g.addEdge(0,1))
    
    // 2) check that all is correct
    g.possibleNodes should be (List(0,1,2))
    g.requiredNodes should be (List(0,1))
    g.requiredEdges(0).sorted  should be (List(0))
    g.requiredEdges(1).sorted  should be (List(0))
    g.requiredEdges(2).sorted  should be (List())
    g.possibleEdges(0).sorted  should be (List(0,1,2,4))
    g.possibleEdges(1).sorted  should be (List(0,2,3,5))
    g.possibleEdges(2).sorted  should be (List(1,3,4,5))
    
    // 3) add constraint
    postAndCheckSuspend(cp,new GraphBipartite(g))

    // 4) check that all changes are correct acc. to definition
    // should change nothing
    g.possibleNodes should be (List(0,1,2))
    g.requiredNodes should be (List(0,1))
    g.requiredEdges(0).sorted  should be (List(0))
    g.requiredEdges(1).sorted  should be (List(0))
    g.requiredEdges(2).sorted  should be (List())
    g.possibleEdges(0).sorted  should be (List(0,1,2,4))
    g.possibleEdges(1).sorted  should be (List(0,2,3,5))
    g.possibleEdges(2).sorted  should be (List(1,3,4,5))
  }
  
  test("Test 2 : Test edges removal") {
    val cp = CPSolver()
    val nnodes : Int = 3
    val g = CPGraphVar(nnodes)(cp)
    // g.edges are (0,1),(0,2),(1,0),(1,2),(2,0),(2,1)
    
    postAndCheckSuspend(cp,g.addEdge(0,1))
    
    // 2) check that all is correct : 
    // check nodes
    g.possibleNodes should be (List(0,1,2))
    g.requiredNodes should be (List(0,1))
    // check edges
    g.requiredEdges(0).sorted  should be (List(0))
    g.requiredEdges(1).sorted  should be (List(0))
    g.requiredEdges(2).sorted  should be (List())
    g.possibleEdges(0).sorted  should be (List(0,1,2,4))
    g.possibleEdges(1).sorted  should be (List(0,2,3,5))
    g.possibleEdges(2).sorted  should be (List(1,3,4,5))
    
    // 3) add constraint
    postAndCheckSuspend(cp,new GraphBipartite(g))

    postAndCheckSuspend(cp,g.addEdge(1,0))
    postAndCheckSuspend(cp,g.addEdge(1,2))
    
    // check that all changes are correct acc. to definition
    // -> should remove edges 1:(0,2) and 4:(2,0) which would create an odd-length cycle if added
    g.possibleNodes should be (List(0,1,2))
    g.requiredNodes should be (List(0,1,2))
    g.requiredEdges(0).sorted  should be (List(0,2))
    g.requiredEdges(1).sorted  should be (List(0,2,3))
    g.requiredEdges(2).sorted  should be (List(3))
    g.possibleEdges(0).sorted  should be (List(0,2))
    g.possibleEdges(1).sorted  should be (List(0,2,3,5))
    g.possibleEdges(2).sorted  should be (List(3,5))
  }
  
  test("Test 3 : Failure at start") {
    // because required Domain is not bipartite
    val cp = CPSolver()
    val nnodes : Int = 3
    val g = CPGraphVar(nnodes)(cp)
    // g.edges are (0,1),(0,2),(1,0),(1,2),(2,0),(2,1)
    
    postAndCheckSuspend(cp,g.addEdge(0,1))
    postAndCheckSuspend(cp,g.addEdge(1,2))
    postAndCheckSuspend(cp,g.addEdge(2,0))
    
    // 3) add constraint
    postAndCheckFailure(cp, new GraphBipartite(g))
  }
  
  test("Test 4 : Success when posting constraint") {
    // because possible Domain is bipartite
    val cp = CPSolver()
    val nnodes : Int = 3
    val edges = List((0,1),(0,2),(2,0))
    val g = CPGraphVar(nnodes, edges)(cp)
     
    val c : Constraint = new GraphBipartite(g)
    c.isActive should be (true)
    // post constraint -> entailment -> constraint no longer active after posted
    postAndCheckSuccess(cp,c)
  }
  
  test("Test 5 : Check correct pruning") {
    val cp = CPSolver()
    val nnodes : Int = 4
    val g = CPGraphVar(nnodes)(cp)
    // g.edges are (0,1),(0,2),(0,3),(1,0),(1,2),(1,3),(2,0),(2,1),(2,3),(3,0),(3,1),(3,2)
    // with index :  0     1     2     3     4     5     6     7     8     9     10    11
    /* We can represent this graph as :
     *      0------1
     *      | \  / |
     *      |  \/  |
     *      |  /\  |
     *      | /  \ |
     *      3------2
     * the dashed lines represent possible edges in both direction (as we have a directed graph) */
    
    postAndCheckSuspend(cp,new GraphBipartite(g))

    //  add some mandatory nodes/edges
    postAndCheckSuspend(cp,g.addEdge(0,1))
    postAndCheckSuspend(cp,g.addEdge(2,3))
    
    // check that all changes are correct acc. to definition :
    g.possibleNodes should be (List(0,1,2,3))
    g.requiredNodes should be (List(0,1,2,3))
    g.requiredEdges(0).sorted  should be (List(0)) // 0:(0,1) is required
    g.requiredEdges(1).sorted  should be (List(0))
    g.requiredEdges(2).sorted  should be (List(8)) // 8:(2,3) is required
    g.requiredEdges(3).sorted  should be (List(8))
    g.possibleEdges(0).sorted  should be (List(0,1,2,3,6,9))
    g.possibleEdges(1).sorted  should be (List(0,3,4,5,7,10))
    g.possibleEdges(2).sorted  should be (List(1,4,6,7,8,11))
    g.possibleEdges(3).sorted  should be (List(2,5,8,9,10,11))
    
    // add another required edge
    postAndCheckSuspend(cp,g.addEdge(1,2))
    
    // check that all changes are correct acc. to definition :
    // as 4:(1,2) is required, we should remove multiple edges :
    //     - remove  1:(0,2) => could create an odd-length cycle
    //     - remove  6:(2,0) => could create an odd-length cycle
    //     - remove  5:(1,3) => could create an odd-length cycle
    //     - remove 10:(3,1) => could create an odd-length cycle

    g.possibleNodes should be (List(0,1,2,3))
    g.requiredNodes should be (List(0,1,2,3))
    g.requiredEdges(0).sorted  should be (List(0)) 
    g.requiredEdges(1).sorted  should be (List(0,4))
    g.requiredEdges(2).sorted  should be (List(4,8)) 
    g.requiredEdges(3).sorted  should be (List(8))
    g.possibleEdges(0).sorted  should be (List(0,2,3,9))
    g.possibleEdges(1).sorted  should be (List(0,3,4,7))
    g.possibleEdges(2).sorted  should be (List(4,7,8,11))
    g.possibleEdges(3).sorted  should be (List(2,8,9,11))
  }
  
}
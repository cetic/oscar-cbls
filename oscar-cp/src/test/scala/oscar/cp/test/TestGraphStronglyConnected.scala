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
import oscar.cp.constraints.GraphStronglyConnected
import oscar.cp.testUtils.TestSuite

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 */

class TestGraphStronlgyConnected extends TestSuite  {
  
  test("Test 1 : Test constraint initial propagation") {
    val cp = CPSolver()
    val nnodes : Int = 3
    val g = CPGraphVar(nnodes)(cp)
    // g.edges are (0,1),(0,2),(1,0),(1,2),(2,0),(2,1)
    
    // 1) add some mandatory nodes/edges
    postAndCheckSuspend(cp,g.addNode(0))
    postAndCheckSuspend(cp,g.addEdge(0,1)) // should also add node 1 as required
    
    // 2) check that all is correct : 
    // check nodes
    g.possibleNodes should be (List(0,1,2))
    g.requiredNodes should be (List(0,1))
    // check edges
    g.requiredEdges(0).sorted    should be (List(0))
    g.requiredEdges(1).sorted    should be (List(0))
    g.requiredEdges(2).sorted    should be (List())
    g.possibleEdges(0).sorted  should be (List(0,1,2,4))
    g.possibleEdges(1).sorted  should be (List(0,2,3,5))
    g.possibleEdges(2).sorted  should be (List(1,3,4,5))
    
    // 3) add constraint
    postAndCheckSuspend(cp,new GraphStronglyConnected(g))
    
    // 4) check that all changes are correct acc. to definition :
    // 		-> nothing to change
    g.possibleNodes should be (List(0,1,2))
    g.requiredNodes should be (List(0,1))
    g.requiredEdges(0).sorted    should be (List(0))
    g.requiredEdges(1).sorted    should be (List(0))
    g.requiredEdges(2).sorted    should be (List())
    g.possibleEdges(0).sorted  should be (List(0,1,2,4))
    g.possibleEdges(1).sorted  should be (List(0,2,3,5))
    g.possibleEdges(2).sorted  should be (List(1,3,4,5))
  }
  
  test("Test 2 : Add required node leading to cutnode") {
    val cp = CPSolver()
    val nnodes : Int = 5
    val edges = List((0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,3),(3,2),(3,4))
    val g = CPGraphVar(nnodes, edges)(cp)
    
    postAndCheckSuspend(cp,g.addNode(0))
    g.possibleNodes should be (List(0,1,2,3,4))
    g.requiredNodes should be (List(0))
    
    postAndCheckSuspend(cp,new GraphStronglyConnected(g))
    // propagation lead to removal of node 4 because not strongly connected and edge (3,4)
    g.possibleNodes should be (List(0,1,2,3))
    g.requiredNodes should be (List(0))
    
    // add node 3 as required
    postAndCheckSuspend(cp,g.addNode(3))
    // propagation lead to changes :
    //	 * as 0 and 3 are required, node 2 is a cutnode in path between 0 and 3
    //     -> set 2 required
    //   * as 0 and 3 are required, edge between 2 and 3 is the only way to go from 3 to 2 and then to 0
    //     -> set edge 6:(2,3) and 7:(3,2) required because it is a bridge
    
    g.possibleNodes.sorted should be (List(0,1,2,3))
    g.requiredNodes.sorted should be (List(0,2,3))
    // check edges : possible = List((0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,3),(3,2))
    g.requiredEdges(0).sorted    should be (List())
    g.requiredEdges(1).sorted    should be (List())
    g.requiredEdges(2).sorted    should be (List(6,7))
    g.requiredEdges(3).sorted    should be (List(6,7))
    g.possibleEdges(0).sorted    should be (List(0,1,2,4))
    g.possibleEdges(1).sorted    should be (List(0,2,3,5))
    g.possibleEdges(2).sorted    should be (List(1,3,4,5,6,7))
    g.possibleEdges(3).sorted    should be (List(6,7))
  }
  
  test("Test 3 : Remove node leading to removal of others") {
    // -> to hold one single connected component
    val cp = CPSolver()
    val nnodes : Int = 5
    val edges = List((0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,3),(3,2),(3,4))
    val g = CPGraphVar(nnodes, edges)(cp)
    
    postAndCheckSuspend(cp,g.addNode(0))
    postAndCheckSuspend(cp,new GraphStronglyConnected(g))
    
    // remove node 2
    postAndCheckSuspend(cp,g.removeNode(2))
    // propagation lead to changes :
    // 	 * two connected components : List(0,1) and List(3) : as 0 is required, remove List(3)
    //   * remove all edges connected with either 2 and 3
    
    g.possibleNodes.sorted should be (List(0,1))
    g.requiredNodes.sorted should be (List(0))
    g.requiredEdges(0).sorted    should be (List())
    g.requiredEdges(1).sorted    should be (List())
    g.requiredEdges(2).sorted    should be (List())
    g.requiredEdges(3).sorted    should be (List())
    g.requiredEdges(4).sorted    should be (List())
    g.possibleEdges(0).sorted    should be (List(0,2))
    g.possibleEdges(1).sorted    should be (List(0,2))
    g.possibleEdges(2).sorted    should be (List())
    g.possibleEdges(3).sorted    should be (List())
    g.possibleEdges(4).sorted    should be (List())
  }
  
  test("Test 4 : Add required node") {
    val cp = CPSolver()
    val nnodes : Int = 5
    val edges = List((0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,3),(3,2),(3,4))
    val g = CPGraphVar(nnodes, edges)(cp)
    
    postAndCheckSuspend(cp,g.addNode(0))
    postAndCheckSuspend(cp,new GraphStronglyConnected(g))
    
    // add node 2 as required
    postAndCheckSuspend(cp,g.addNode(2))
    // propagation lead to no changes other than add node 2
    
    g.possibleNodes.sorted should be (List(0,1,2,3))
    g.requiredNodes.sorted should be (List(0,2))
    for (n <- 0 to nnodes-1)
      g.requiredEdges(n).sorted    should be (List())
    g.possibleEdges(0).sorted    should be (List(0,1,2,4))
    g.possibleEdges(1).sorted    should be (List(0,2,3,5))
    g.possibleEdges(2).sorted    should be (List(1,3,4,5,6,7))
    g.possibleEdges(3).sorted    should be (List(6,7))
  }
  
  test("Test 5 : Try to remove a bridge") {
    // should lead to failure
    val cp = CPSolver()
    val nnodes : Int = 5
    val edges = List((0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,3),(3,2),(3,4))
    val g = CPGraphVar(nnodes, edges)(cp)
    
    postAndCheckSuspend(cp,g.addNode(0))
    postAndCheckSuspend(cp,new GraphStronglyConnected(g))
    
    // add node 3 as required
    postAndCheckSuspend(cp,g.addNode(3))
    // propagation lead to changes :
    //	 * as 0 and 3 are required, node 2 is a cutnode in path between 0 and 3
    //     -> set 2 required
    //   * as 0 and 3 are required, edge between 2 and 3 is the only way to go from 3 to 2 and then to 0
    //     -> set edge 3:(2,3) required because it is a bridge
    
    postAndCheckFailure(cp, g.removeEdge(2,3))
    cp.isFailed should be (true)
  }
  
  test("Test 6 : Add required node") {
    // add two nodes in a graph containing only these two
    val cp = CPSolver()
    val nnodes : Int = 2
    val edges = List((0,1),(1,0))
    val g = CPGraphVar(cp, nnodes, edges)
    
    postAndCheckSuspend(cp,g.addNode(0))
    postAndCheckSuspend(cp,new GraphStronglyConnected(g))
    
    // add node 1 as required
    postAndCheckSuspend(cp,g.addNode(1))
    // propagation lead to changes :
    //   * as 0 and 1 are required, edge between them is the only way to go from one to another
    //     -> set edge 0:(0,1) required because it is a bridge
    
    g.possibleNodes.sorted should be (List(0,1))
    g.requiredNodes.sorted should be (List(0,1))
    g.requiredEdges(0).sorted    should be (List(0,1))
    g.requiredEdges(1).sorted    should be (List(0,1))
    g.possibleEdges(0).sorted    should be (List(0,1))
    g.possibleEdges(1).sorted    should be (List(0,1))
  }
  
  test("Test 7 : Try to add required node") {
    // try to add two nodes in a graph containing only these two
    // 	 but g dont have two directions edges
    val cp = CPSolver()
    val nnodes : Int = 2
    val edges = List((0,1))
    val g = CPGraphVar(cp, nnodes, edges)
    
    postAndCheckSuspend(cp,g.addNode(0))
    postAndCheckSuspend(cp,new GraphStronglyConnected(g))
    // propagation leads to removal of node 1 
    //   -> 0 and 1 are not strongly connected and 0 is required
    
    // try to add node 1 as required
    postAndCheckFailure(cp, g.addNode(1))
    
    g.possibleNodes.sorted should be (List(0))
    g.requiredNodes.sorted should be (List(0))
    g.requiredEdges(0).sorted    should be (List())
    g.requiredEdges(1).sorted    should be (List())
    g.possibleEdges(0).sorted    should be (List())
    g.possibleEdges(1).sorted    should be (List())
  }
}
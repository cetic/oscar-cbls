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


package oscar.cp.constraints

import oscar.cp.core._

import scala.collection.mutable.PriorityQueue
import oscar.algo.reversible.ReversibleBoolean
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.variables.{CPGraphVar, CPIntVar, CPVar}

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 * Defines the constraint shortest Path(g, n1, n2, w, I) for a directed graph :
 * 	this constraint holds if there is a path from n1 to n2 (n1 != n2) in the graph g of total weight lower or equal to I  
 *  @param  g   the graph
 *          src  the source of the path
 *          dest the destination of the path
 *          w   the function : (u,v) => weight(u,v) for all edge in the graph g
 *          I   the maximal path weight
 */

class GraphPath(val g : CPGraphVar, src : Int, dest : Int, w : (Int,Int) => Int, I : CPIntVar) extends Constraint(g.s, "Path") {
  override def associatedVars(): Iterable[CPVar] = Array(g, I)

  val pNodes : List[Int] = g.possibleNodes
    val n : Int = pNodes.length
    // build transitive closure of the possible values of the graph
    //   tc is composed of ReversibleBool to allow backtrack in search and still update tc
    var tc : Array[Array[ReversibleBoolean]] = buildTC(pNodes)
    // count the number of possible edges/nodes to be able to detect modification when propagate is called
    var nbEdges : ReversibleInt = new ReversibleInt(g.s, g.nbPossibleEdges)
    var nbNodes : ReversibleInt = new ReversibleInt(g.s, n)
  
    override def setup(l: CPPropagStrength): Unit = {
      // add src and dest mandatory
      g.addNodeToGraph(src)
      g.addNodeToGraph(dest)
      // remove all edges incoming to the source and all outgoing edges from the destination (they must not be used to make a path from src to dest)
      for (e <- g.possibleInEdges(src))
        g.removeEdgeFromGraph(e)
      for (e <- g.possibleOutEdges(dest))
        g.removeEdgeFromGraph(e)
      
	  // add filter when domain changes
	  g.callPropagateWhenDomainChanges(this)
      
	  // launch initial propagation
	  propagate()
	}
    
    override def propagate(): Unit = {
      val possNodes : List[Int] = g.possibleNodes
      val reqNodes  : List[Int] = g.requiredNodes
       
      // check if the constraint is entailed (there is a required path)
      isEntailed(possNodes,reqNodes)
      if(!isActive)
        return
      
      // check if we need to rebuild tc before pruning
      val newNbEdges : Int = g.nbPossibleEdges
      val newNbNodes : Int = possNodes.length
      if (newNbEdges < nbEdges.getValue || newNbNodes < nbNodes.getValue) {
        // at least one edge/node was removed since last propagate -> update nbEdges/nbNodes and update TC
        nbEdges.setValue(newNbEdges)
        nbNodes.setValue(newNbNodes)
        updateTC(possNodes, reqNodes)
      }

      // pruning
	  
      // 1) prune upper bound : forbidden edges
      // an arc (u,v) is forbidden if 
      //	   (src,u) not in TC
      //    OR (v,dest) not in TC
      //    OR exists n in requiredNodes such that : (n,u) not in TC AND (v,n) not in TC
      val possibleEdges : List[Int] = possNodes.flatMap(g.possibleOutEdges(_))
      for (e <- possibleEdges){
        val (u,v) = g.edge(e)
	    if (tc(src)(u).getValue == false){
	      // remove e because no path from src to u in tc
	      g.removeEdgeFromGraph(e)
	    } else if (tc(v)(dest).getValue == false) {
	      // remove e because no path from v to dest in tc
	      g.removeEdgeFromGraph(e)
	    } else {
	      var b : Boolean = true
	      for (n <- reqNodes; if b){
	        if (tc(n)(u).getValue == false && tc(v)(n).getValue == false){
	          // remove e because no path from src to dest passing by required node n using e
	          g.removeEdgeFromGraph(e)
	          b = false // edge is removed, no need to look for it again
	        }
	      }
	    }
	  }
      // for each required edge,
      // 	we should remove its counterpart, all other edges starting from its source and all other edges which destination is the same
      val requiredEdges : List[Int] = possNodes.flatMap(g.requiredOutEdges(_))
      val posNotReqEdgesTuple : List[(Int,Int)] = possNodes.flatMap(g.possibleOutEdges(_)).filter(!requiredEdges.contains(_)).map(g.edge(_))
      for (req <- requiredEdges){
        val (u,v) = g.edge(req)
        // counterpart edge
        if(posNotReqEdgesTuple.contains((v,u)))
          g.removeEdgeFromGraph(v,u)
        // all edges starting from same source or with same destination
        val toBeRemovedEdges : List[(Int,Int)] = posNotReqEdgesTuple.filter(e => (e._1 == u) || (e._2 == v))
        for (e <- toBeRemovedEdges) g.removeEdgeFromGraph(e._1,e._2)
      }
      
      // 2) prune according to I : forbidden edges
      //   compute minimal distance d(n1,n), d(n,n2) for each node n of the graph
      //   edge (u,v) is forbidden if (d(n1,u) + w(u,v) + d(v,n2)) > I
      val (mind1, mind2) : (Array[Int], Array[Int]) = buildDist(src,dest,possNodes)
      val newPossibleEdges : List[Int]= possNodes.flatMap(g.possibleOutEdges(_))
      for (e <- newPossibleEdges) {
        val (u,v) = g.edge(e)
        if (mind1(u) + w(u,v) + mind2(v) > I.max)
          g.removeEdgeFromGraph(e)
      }
      
      // 3) prune lower bound : mandatory edges
      for (n1 <- reqNodes; if (g.requiredOutEdges(n1).length == 0)){
        if (g.possibleOutEdges(n1).length == 1){
          // there is a bridge (n1,n2) as n1 is mandatory and has only one outgoing edge
          val n2 : Int = g.edge(g.possibleOutEdges(n1).head)._2
          g.addEdgeToGraph(n1,n2)
        }
      }
      
      // 4) prune upper bound : forbidden nodes
      // isolated nodes are forbidden : if a node is not the destination (target) and does not have outgoing edges, it should be removed
      for (node <- possNodes; if node != dest) {
        if (g.possibleOutEdges(node).isEmpty)
          g.removeNodeFromGraph(node)
      }
      
      // 5) prune lower bound : mandatory nodes
      val possibleNotMandatoryNodes = g.possibleNodes.filter(!reqNodes.contains(_))
      // we have to check that, if we remove a possible node; 
      //   all mandatory nodes are still reachable from src
      //   and dest can be reachable from all mandatory nodes
      for (n <- possibleNotMandatoryNodes){
        if (checkMandatoryNodes(src, dest, n))
          g.addNodeToGraph(n)
      }
      
      // update bounds
      val (minDist, maxDist) : (Int,Int) = computeBounds
      if (minDist==maxDist){
        isEntailed(g.possibleNodes, g.requiredNodes)
      }
      else {
        if (minDist > I.min)
          I.updateMin(minDist)
    	  if (maxDist < I.max)
          I.updateMax(maxDist)
      }
    }
	
	
  /* ---------------------------------- 
   * Useful functions to compute the constraint
   * --------------------------------- */
    
    
    // check if there exists a required path ( = path composed of only required edges from source to destination)
    // 	if it is true, the maximal weight path CPIntVar is also entailed
    private def isEntailed(possNodes : List[Int], reqNodes : List[Int]) : Unit = {
      if (existRequiredPath(src, dest, reqNodes)){
        val requiredEdges : List[Int] = reqNodes.flatMap(g.requiredOutEdges(_))
        val possibleNotRequiredEdges : List[Int] = possNodes.flatMap(g.possibleOutEdges(_)).filter(!requiredEdges.contains(_))
        
        // remove all other possible edges
        for (e <- possibleNotRequiredEdges)
          g.removeEdgeFromGraph(e)
        
        // assign bound
        val nodesInPath = reqNodes.filter(_ != dest)
        val pathLen : Int = nodesInPath.map(i => g.requiredOutEdges(i).map(j => w(g.edge(j)._1,g.edge(j)._2)).head).sum
        I.assign(pathLen)
        deactivate()
      }
    }    
        
    private def requiredNeighborsList(nodeId : Int) : List[Int] = g.requiredOutEdges(nodeId).map(g.edge(_)._2)
          
    // return true if there is a required path from s to d of length equal to all required nodes
    private def existRequiredPath(s : Int, d : Int, reqNodes: List[Int]) : Boolean = {
      // Search to find a path from s to d
      if (s == d) return true
      var toVisit : List [Int] = List(s)
      var visited : List[Int] = List()
      val nbReqNodes = reqNodes.length
      while (! toVisit.isEmpty) {
        val node : Int = toVisit.head
        visited = node :: visited
        val neighb =  requiredNeighborsList(node).filter(!visited.contains(_))
        if (neighb.contains(d) && (visited.length + 1) == nbReqNodes) return true
        toVisit = toVisit.tail ++ neighb
      }
      return false
    }
	
    // build transitive closure of possible edges of the graph
    private def buildTC(possNodes : List[Int]) : Array[Array[ReversibleBoolean]] = {
      val tempTc =  Array.fill(n)(Array.fill(n)(new ReversibleBoolean(g.s,true)))
      // precompute possible neighbors to spare computation time and give it to existsPath()
      val possNeigh : List[List[Int]] = (0 until n).toList.map(possibleNeighborsList(_))
      // update tc
      for (n1 <- possNodes){
        for (n2 <- possNodes; if n1 != n2){
            if (!existPath(n1, n2, possNeigh))
              tempTc(n1)(n2).setValue(false)
        }
      }
      tempTc
    }
    
    // update TC with nodes that might accordingly
    private def updateTC(possNodes : List[Int], reqNodes : List[Int]) : Unit = {
      // compute only nodes that might change
      val srcReqNodes = reqNodes.flatMap(g.requiredOutEdges(_)).map(g.edge(_)._1)
      val nodesToCheck : List[Int] = possNodes.filter(!srcReqNodes.contains(_))
      // precompute possible neighbors to spare computation time and give it to existsPath()
      val possNeigh : List[List[Int]] = (0 until n).toList.map(possibleNeighborsList(_))
      // update tc
      for (n1 <- nodesToCheck){
        for (n2 <- nodesToCheck; if n1 != n2){
          if (tc(n1)(n2).getValue != false) { // if they were already no available path, no reason to go further
            if (!existPath(n1, n2, possNeigh))
              tc(n1)(n2).setValue(false)
          }
        }
      }
    }
    
    // get a list of possible neighbors of the node nodeId : neighbors are outgoing endPoints of edges starting from nodeId
    private def possibleNeighborsList(nodeId : Int) : List[Int] = g.possibleOutEdges(nodeId).map(g.edge(_)._2)
    // get a list of possible in-neighbors of the node nodeId: neighbors are incoming starting endPoints of edges whose destination is nodeId
    private def possibleInNeighborsList(nodeId : Int) : List[Int] = g.possibleInEdges(nodeId).map(g.edge(_)._1)
          
    // return true if there exists at least one path from s to d
    private def existPath(s : Int, d : Int, possNeigh : List[List[Int]]) : Boolean = {
      // Search to find a path from s to d
      if (s == d) return true
      var toVisit : List[Int] = List(s)
      var visited : List[Int] = List()
      while (! toVisit.isEmpty) {
        val node : Int = toVisit.head
        val possNList = possNeigh(node)
        if (possNList.contains(d)) return true
        visited = node :: visited
        val neighb =  possNList.filter(!visited.contains(_))  
        toVisit = neighb ::: toVisit.tail
      }
      return false
    }
    
    // return true if there exists at least one path from s to d without going through n
    private def existPath(s : Int, d : Int, n : Int) : Boolean = {
      // Search to find a path from s to d
      if (s == d) return true
      var toVisit : List [Int] = List(s)
      var visited : List[Int] = List(n) // we will not visit n anymore
      while (! toVisit.isEmpty) {
        val node : Int = toVisit.head
        visited = node :: visited
        if (node == d) return true
        val neighb =  possibleNeighborsList(node).filter(!visited.contains(_))
        toVisit = toVisit.tail ++ neighb
      }
      return false
    }
    
    // we have to check that, if we remove a possible node; 
    //   all mandatory nodes are still reachable from src
    //   and dest can be reachable from all mandatory nodes
    private def checkMandatoryNodes(s : Int, d : Int, n : Int) : Boolean = {
      val mandatoryNodes = g.requiredNodes
      for (mandNode <- mandatoryNodes){
        if (!existPath(s, mandNode, n)){
        	// there is no path from source s to a mandatory node manNode without going through n
        	return true  // n should be mandatory
        }
        if (!existPath(mandNode, d, n)){
        	// there is no path from a mandatory node manNode to the destination d without going through n
        	return true  // n should be mandatory
        }
      }
      return false    
    }
    
    // build distances (shortest path) from n1 and n2 towards all other nodes of the graph
    // return two Arrays of distances from n1 to other nodes and n2 respectively
    private def buildDist(n1 : Int, n2 : Int, possNodes : List[Int]) : (Array[Int], Array[Int]) = {
      (getMinDistFromSource(n1),getMinDistToDestination(n2))
    }
    
    // follow Dijkstra algorithm to get minimal distance from the source s to all possible nodes
    // @return an array of distances from the source to each node
    private def getMinDistFromSource(s : Int) : Array[Int] = {
      // init
      val possNeigh : List[List[Int]] = (0 until n).toList.map(possibleNeighborsList(_))     
      var distances = Array.fill(n)(Int.MaxValue)
      // create class node with a value (node number) and a distance dist to be used in the PriorityQueue
      case class Node(value : Int, dist : Int) extends Ordered[Node] {
        def compare(node: Node)= node.dist compare this.dist
      }
      var queue = PriorityQueue[Node]()
      val visited = Set[Int]()
      
      // start with the source s and a distance of 0
      queue.enqueue( Node(s,0) )
      distances(s) = 0
      
      // iterate until all nodes are visited
      while (!queue.isEmpty) {
        val currNode : Node = queue.dequeue // the node from q with the smallest dist
        val node = currNode.value
        
        visited + (node)
        // get all non already visited neighbor of currentNode and try to find new minimal distances
        for (neighb <- possNeigh(node); if ! visited.contains(neighb) ) {
          val newdist : Int = distances(node) + w(node,neighb)
          if ( newdist < distances(neighb) ) { // update distance and add to queue
            distances(neighb) = newdist
            queue.enqueue( Node(neighb,newdist) )
          }
        }
      }
      
      distances
    }
    
    // follow Dijkstra algorithm in reverse to get minimal distance from all nodes to destination d
    //   we start from the destination and visit all nodes through edges in reverse order
    //   e.g. if there is an edge (i,d), i is an in-neighbor of d and we will visit i from d
    // @return an array of distances from the source to each node
    private def getMinDistToDestination(d : Int) : Array[Int] = {
      // init
      val possInNeigh : List[List[Int]] = (0 until n).toList.map(possibleInNeighborsList(_))
      var distances = Array.fill(n)(Int.MaxValue)
      // create class node with a value (node number) and a distance dist to be used in the PriorityQueue
      case class Node(value : Int, dist : Int) extends Ordered[Node] {
        def compare(node: Node)= node.dist compare this.dist
      }
      var queue = PriorityQueue[Node]()
      val visited = Set[Int]()
      
      // start with the destination d and with a distance of 0
      queue.enqueue( Node(d,0) )
      distances(d) = 0
      
      // iterate until all the possible nodes are visited
      while (!queue.isEmpty) {
        val currNode : Node = queue.dequeue // the node from q with the smallest dist
        val node = currNode.value
        
        visited + (node)
        // get all non already visited neighbor of currentNode and try to find new minimal distances
        //   as we are doing a reverse Dijkstra (from destination to source), neighbors are incoming edges of the node
        for (neighb <- possInNeigh(node); if ! visited.contains(neighb) ) {
          val newdist : Int = distances(node) + w(neighb,node)
          if ( newdist < distances(neighb) ) { // update distance and add to queue
            distances(neighb) = newdist
            queue.enqueue( Node(neighb,newdist) )
          }
        }
      }

      distances
    }

    // compute the weight of the minimal spanning tree (MST)
    private def minSpanTreeWeight() : Int = {
      var connectedComponentsOfVisitedNodes : List[List[Int]] = List(List())
      
      def checkAndReturn(ccList : List[List[Int]], ni : Int) : (List[Int],List[List[Int]]) = {
        // check if ni is in a connected component and return (the component, the connectedComponentList without this one)
        var checked : List[List[Int]] = List(List())
        var toCheck : List[List[Int]] = ccList
        while (!toCheck.isEmpty){
          val comp = toCheck.head
          toCheck = toCheck.tail
          if (comp.contains(ni)){
            return (comp,checked ++ toCheck)
          } else {
            checked = comp :: checked
          }
        }
        (List(),List(List())) // ni not in any connected component
      }
      def addConnectEdge(n1 : Int, n2 : Int): Unit = {
        // Determine if in a component
        val (comp1, ccList1) : (List[Int],List[List[Int]]) = checkAndReturn(connectedComponentsOfVisitedNodes,n1)
        val (comp2, ccList2) : (List[Int],List[List[Int]]) = checkAndReturn(connectedComponentsOfVisitedNodes,n2)
        if (comp1.isEmpty){ // n1 not in connectedComponentList
          if (comp2.isEmpty) { // n2 not in connectedComponentList, add a new component
            connectedComponentsOfVisitedNodes = List(n1,n2) :: connectedComponentsOfVisitedNodes
          } else { // n2 in a component, add n1 to component2
            connectedComponentsOfVisitedNodes = (n1 :: comp2) :: ccList2
          }
        } else {
          if (comp2.isEmpty) { // n2 not in connectedComponentList, add n2 to component1
            connectedComponentsOfVisitedNodes = (n2 :: comp1) :: ccList1
          } else { // n1,n2 are in two different components, mix them
            connectedComponentsOfVisitedNodes = (comp1 ++ comp2) :: (ccList1 diff List(comp2))
          }
        }
      } 
      
      val reqNodes = g.requiredNodes
      var possEdges : List[(Int,Int)] = reqNodes.flatMap(g.possibleOutEdges(_)).map(g.edge(_))
      // possible edges for the spanning tree are edges that have both endPoints required
      possEdges = possEdges.filter(e => reqNodes.contains(e._1) && reqNodes.contains(e._2))
      val reqEdges : List[(Int,Int)] = reqNodes.flatMap(g.requiredOutEdges(_)).map(g.edge(_))  
      var sumWeights = 0
      
      // build connected components with required edges or the minimal possible edge if no requiredEdges
      if (reqEdges.isEmpty){
        if (possEdges.isEmpty) return 0 // no edges
        val edge = possEdges.minBy(t => w(t._1,t._2))
        possEdges = possEdges diff List(edge)
        addConnectEdge(edge._1, edge._2)
        sumWeights += w(edge._1, edge._2) 
      } else{
        reqEdges.foreach(e => addConnectEdge(e._1, e._2))
        reqEdges.foreach(e => sumWeights += w(e._1, e._2))
      }  
       
      // update possEdges: remove edges that can create a cycle if they were added
      //  In other words, remove edges such that both endPoints are in the same connected component (adding it would create a cycle)
      possEdges = possEdges.filter(t => !connectedComponentsOfVisitedNodes.exists(comp => comp.contains(t._1) && comp.contains((t._2))))
      
      // add minimal edge at each iteration and update accordingly
      while (!possEdges.isEmpty){
        val edge : (Int,Int) = possEdges.minBy(t => w(t._1,t._2))
        // add to edgesVisited
        sumWeights += w(edge._1,edge._2)
        addConnectEdge(edge._1, edge._2)
        // remove edges from possEdges creating cycle,
        //     check only first connectedComponents because it the modified one in addConnectEdge()
        possEdges = possEdges.filter(t => !(connectedComponentsOfVisitedNodes.head.contains(t._1) && connectedComponentsOfVisitedNodes.head.contains((t._2))))
      }
      
      sumWeights
    }
    
    private def computeBounds() : (Int,Int) = {
      val lmin = g.requiredNodes.map(g.possibleOutEdges(_))
      val minH = lmin.map(innerList => if (innerList.isEmpty) 0 else innerList.map(e => w(g.edge(e)._1,g.edge(e)._2)).min).sum
      val minSpan = minSpanTreeWeight // efficient when all nodes are mandatory
      val minB = List(minH,minSpan).max
      
      val lmax = g.possibleNodes.map(g.possibleOutEdges(_))
      val maxB = lmax.map(innerList => if (innerList.isEmpty) 0 else innerList.map(e => w(g.edge(e)._1,g.edge(e)._2)).max).sum
      
      (minB, maxB)
    }
}
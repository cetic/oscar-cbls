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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/

package oscar.cbls.algo.dag

import oscar.cbls.algo.heap.BinomialHeap
import oscar.cbls.algo.quick.QList

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

/** a DAG node with some abstract methods
  * @author renaud.delandtsheer@cetic.be
  */
trait DAGNode extends Ordered[DAGNode]{

  /**the position in the topological sort*/
  var position: Int = 0

  /**supposed to be false between each pass of the algorithm*/
  var visited: Boolean = false

  /**it gives the unique ID of the PropagationElement.
    * those uniqueID are expected to start at 0L and to increase continuously
    * An exception is tolerated: uniqueID is set to -1L
    * if the Propagation Element is not mentioned in the propagation structure, such as for constants
    * yet is mentioned in the dependencies of registered propagation elements
    */
  var uniqueID:Int = -1

  protected[dag] def getDAGPrecedingNodes: Iterable[DAGNode]

  protected[dag] def getDAGSucceedingNodes: Iterable[DAGNode]
}

/**
 * @author renaud.delandtsheer@cetic.be
 * @param n a node that is involved in the cycle
 */
class CycleException(n: DAGNode) extends Exception

/**This data structure performs dynamic topological sort on DAG
  * the topological sort can be performed either from scratch or maintained incrementally.
  * The topological sort is about maintaining the attribute Position in the nodes [[oscar.cbls.algo.dag.DAGNode]]
  *
  * the topological sort is lower before
  *
  * The incremental topological sort in _autoSort(mAutoSort: Boolean){
  *
  * @author renaud.delandtsheer@cetic.be
  */
trait DAG {
  private var AutoSort: Boolean = false

  def nodes:Iterable[DAGNode]

  /**performs a self-check on the ordering, use for testing*/
  def checkSort(): Unit = {
    for (to <- nodes){
      for(from <- to.getDAGPrecedingNodes){
        assert(from.position < to.position,"topological sort is wrong at " + from + "->" + to)
      }
    }
    for (from <- nodes){
      for(to <- from.getDAGSucceedingNodes){
        assert(from.position < to.position,"topological sort is wrong at " + from + "->" + to)
      }
    }
  }

  /**Checks that node have correct reference to each other.
    * Nodes are expected to know their successors and predecessors.
    * This is expected to be consistent between several nodes.
    */
  def checkGraph(): Unit = {
    nodes.foreach(n => {
      n.getDAGPrecedingNodes.foreach(p=> {
        if(!p.getDAGSucceedingNodes.exists(p => p == n)){
          throw new Exception("graph is incoherent at nodes [" + p + "] -> [" + n +"]")
        }
      })

      n.getDAGSucceedingNodes.foreach(p=> {
        if(!p.getDAGPrecedingNodes.exists(p => p == n)){
          throw new Exception("graph is incoherent at nodes [" + n + "] -> [" + p +"]")
        }
      })
    })
  }

  /**turns the incremental sort on or off.
    * Incremental sort is then applied at each edge insert. node insert and delete is prohibited when autosort is activated
    * in case a cycle is detected, does not pass in autosort model, but throws an exception
    */
  def autoSort_=(mAutoSort: Boolean): Unit = {
    if (mAutoSort && !AutoSort) {
      try{
        doDAGSort()
      }catch {
        case e:CycleException =>
          throw new Error("cycle in topological sort: \n " + getCycle().mkString("\n ") + "\n")
      }
      assert({checkSort(); checkGraph(); true})
      //will throw an exception in case of cycle, so AutoSort will not be set to true
      AutoSort = true
    } else if (AutoSort && ! mAutoSort) {
      //on sort de l'autosort
      AutoSort = false
    }
  }

  /**@return the autosort status*/
  def autoSort:Boolean = AutoSort

  /**to notify that an edge has been added between two nodes.
    * this will trigger a re-ordering of the nodes in the topological sort if it is activated.
    * The reordering might lead to an exception [[oscar.cbls.algo.dag.CycleException]] in case there is a cycle in the graph
    * We expect the graph to be updated prior to calling this method
    * notice that you do not need to notify edge deletion.
    */
  def notifyAddEdge(from: DAGNode, to: DAGNode): Unit = {

    if (AutoSort && (from.position > to.position)) {
      //refaire le sort
      //discovery

      val SortedForwardRegion = findSortedForwardRegion(to, from.position)
      val SortedBackwardsRegion = findSortedBackwardRegion(from, to.position)

      //reassignment

      val FreePositionsToDistribute: QList[Int] = mergeNodeLists(SortedForwardRegion, SortedBackwardsRegion)

      val FreePositionsForForwardRegion = realloc(SortedBackwardsRegion, FreePositionsToDistribute)
      realloc(SortedForwardRegion, FreePositionsForForwardRegion )

      assert({checkSort(); checkGraph(); true})
    }
  }

  //retourne un cycle, pour aider au debugging
  //pre: il y a un cycle dans le Algo
  //argument optionel: un noeud implique dans le cycle: on commence par chercher un cycle impliquant ce noeud.
  //si pas de cycle, retourne null.
  def getCycle(Start:DAGNode=null):List[DAGNode] = {

    //on marque visite quand on poppe de la DFS ou quand on est retombe sur le debut du cycle
    var ExploredStack:List[DAGNode] = List.empty //upside down

    var visited2:SortedSet[Long] = SortedSet.empty

    def DFS(n:DAGNode):Boolean = { //return true si on a trouve un cycle
      if(n.visited) return false
      if(visited2.contains(n.uniqueID)){  //found a cycle
        ExploredStack = (n :: ExploredStack).reverse
        n.visited=true
        while(!ExploredStack.head.visited){ExploredStack = ExploredStack.tail}
        nodes.foreach(p => {p.visited = false; visited2 -= p.uniqueID})
        true
      }else{ //not yet
        visited2 += n.uniqueID
        ExploredStack = n :: ExploredStack
        n.getDAGSucceedingNodes.foreach(p => {if(DFS(p)){return true}})
        n.visited=true
        visited2 -= n.uniqueID
        ExploredStack = ExploredStack.tail
        false
      }
    }

    if(Start != null){
      if(DFS(Start)){ return ExploredStack }
      else return List(Start)
    }
    nodes.foreach(n => {
      if (!n.visited)
        if (DFS(n)){return ExploredStack}
    })
    nodes.foreach(p => {p.visited = false})
    List.empty
  }

  /**sorts DAG nodes according to dependencies.
    * first position is set to zero.
    * this throws an exception [[oscar.cbls.algo.dag.CycleException]] in case a cycle is detected
    */
  def doDAGSort(): Unit = {
    //on utilise les positions pour stocker le nombre de noeuds predecesseurs non visites, puis on met l'autre valeur apres.
    var front: QList[DAGNode] = null
    nodes.foreach(n => {
      val pos = - n.getDAGPrecedingNodes.size
      n.position = pos
      if(pos == 0) front = QList(n,front)
    })

    var position = 0 //la position du prochain noeud place.
    while (front != null) {
      val n = front.head
      front = front.tail
      n.position = position
      position += 1
      n.getDAGSucceedingNodes.foreach(p => {
        p.position +=1
        if (p.position == 0) front = QList(p,front) //une stack, en fait, mais c'est insensitif, puis c'est plus rapide.
      })
    }
    if (position != nodes.size) {
      throw new CycleException(null)
    }
  }

  /*
  private def findForwardRegion(n: DAGNode, ub: Long): List[DAGNode] = {
    def dfsF(n: DAGNode, acc: List[DAGNode]): List[DAGNode] = {
      n.visited = true
      var newlist = n :: acc
      n.getDAGSucceedingNodes.foreach(p => {
        if (p.position == ub) {
          nodes.foreach(q => q.visited = false)
          throw new CycleException(p)
        }
        if (!p.visited && p.position < ub) {
          newlist = dfsF(p, newlist)
        }
      })
      newlist
    }
    dfsF(n, List.empty)
  }
*/
  val HeapForRegionDiscovery:BinomialHeap[DAGNode] = new BinomialHeap((n:DAGNode) => n.position,nodes.size)

  /**@return forward region, sorted by increasing position*/
  private def findSortedForwardRegion(n: DAGNode, ub: Long): QList[DAGNode] = {

    val h:BinomialHeap[DAGNode] = HeapForRegionDiscovery
    h.dropAll()
    h.keyGetter = (n:DAGNode) => n.position

    var toreturn:QList[DAGNode] = null

    h.insert(n)
    n.visited = true

    while(!h.isEmpty){
      val first:DAGNode = h.popFirst()
      toreturn = QList(first,toreturn)
      first.getDAGSucceedingNodes.foreach((p:DAGNode) => {
        if (p.position == ub) {
          toreturn.foreach(q => q.visited = false)
          h.foreach(q => q.visited = false)
          throw new CycleException(p)
        }
        if (!p.visited && p.position < ub) {
          h.insert(p)
          p.visited = true
        }
      })
    }
    toreturn.reverse
  }

  /*
  private def findBackwardsRegion(n: DAGNode, lb: Long): List[DAGNode] = {
    def dfsB(n: DAGNode, acc: List[DAGNode]): List[DAGNode] = {
      n.visited = true
      var newlist = n :: acc
      n.getDAGPrecedingNodes.foreach(p => {
        if (!p.visited && p.position > lb) {
          newlist = dfsB(p, newlist)
        }
      })
      newlist
    }
    dfsB(n, List.empty)
  }
*/
  /**@return forward region, sorted by increasing position*/
  private def findSortedBackwardRegion(n: DAGNode, lb: Long): QList[DAGNode] = {

    val h:BinomialHeap[DAGNode] = HeapForRegionDiscovery
    h.dropAll()
    h.keyGetter = (n:DAGNode) => -n.position

    var toreturn: QList[DAGNode] = null

    h.insert(n)
    n.visited = true

    while(!h.isEmpty){
      val first = h.popFirst()
      toreturn = QList(first,toreturn)

      first.getDAGPrecedingNodes.foreach(p => {
        if (!p.visited && p.position > lb) {
          h.insert(p)
          p.visited = true
        }
      })
    }
    toreturn
  }


  //merge deux listes de noeuds triee par position, donne la position triee de ces noeuds
  private def mergeNodeLists(a: QList[DAGNode], b: QList[DAGNode]): QList[Int] = {
    if (a == null && b == null){
      null
    }else if (a == null) {
      QList(b.head.position, mergeNodeLists(a, b.tail))
    } else if (b == null) {
      QList(a.head.position,mergeNodeLists(a.tail, b))
    } else if (a.head.position < b.head.position) {
      QList(a.head.position,mergeNodeLists(a.tail, b))
    } else {
      QList(b.head.position,mergeNodeLists(a, b.tail))
    }
  }

  @tailrec
  private def realloc(OrderedNodeForReinsertion: QList[DAGNode], FreePositionsToDistribute: QList[Int]):QList[Int] = {
    if (OrderedNodeForReinsertion != null) {
      OrderedNodeForReinsertion.head.visited = false
      OrderedNodeForReinsertion.head.position = FreePositionsToDistribute.head
      realloc(OrderedNodeForReinsertion.tail, FreePositionsToDistribute.tail)
    }else{
      FreePositionsToDistribute
    }
  }
}

package oscar.cp.scheduling.precedencegraph

import oscar.algo.Inconsistency
import oscar.cp.scheduling.precedencegraph.datastructures.{ActivityQueue, ActivityStack, IncreasingSet, ReversibleSetOfActivities}
import oscar.cp.{CPIntVar, _}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by saschavancauwelaert on 27/04/16.
  */

class PrecedenceGraph(starts:Array[CPIntVar], durations:Array[CPIntVar], ends: Array[CPIntVar], ttMatrix: Array[Array[Int]], initialKnownPrecedences: Array[(Int,Int)] = Array()) {

  implicit private[this] val _cp = starts(0).store
  private[this] val _nTasks = starts.length
  private[this] val tasks = Array.tabulate(_nTasks)(t => t)

  val store = _cp
  val nTasks = _nTasks

  val nonDetectableSuccessorsContainers : Array[Array[Int]] = Array.ofDim(_nTasks, _nTasks - 1)
  val nonDetectablePredecessorsContainers : Array[Array[Int]] = Array.ofDim(_nTasks, _nTasks - 1)

  private[this] val EMPTY_NODE = -1

  /*** Internal Structures ***/
  private[this] val tasksInNonDetectablePrecsContainer : Array[Int] = Array.ofDim(_nTasks)
  private[this] val postOrderArray = Array.fill(_nTasks)(EMPTY_NODE)
  private[this] val topoOrder = Array.fill(_nTasks)(EMPTY_NODE)
  private[this] val postOrder = new ActivityQueue(_nTasks)
  private[this] val successorsInTransitiveClosure = Array.tabulate(_nTasks)(v => new IncreasingSet(v, _nTasks))
  private[this] val successorsInTransitiveClosureContainers : Array[Array[Int]] = Array.ofDim(_nTasks, _nTasks)
  private[this] val UNSEEN = -1
  private[this] val EXPANDED = 0
  private[this] val CLOSED = 1
  private[this] val nodeStatus = Array.fill(_nTasks)(UNSEEN)
  private[this] val stack = new ActivityStack(1000)

  /*** Non Detectable Precedences ***/
  private[this] val nonDetectableSuccessors : Array[ReversibleSetOfActivities] = Array.fill(_nTasks)(new ReversibleSetOfActivities(_cp,  _nTasks))
  private[this] val nonDetectablePredecessors : Array[ReversibleSetOfActivities] = Array.fill(_nTasks)(new ReversibleSetOfActivities(_cp,  _nTasks))
  private[this] val tasksInNonDetectablePrecs = new ReversibleSetOfActivities(_cp, _nTasks)

  private[this] var associatedPropagators = ArrayBuffer[Constraint]()

  //add the initial non detectable precedences
  for(precs <- initialKnownPrecedences)(addNonDetectablePrec(precs._1, precs._2))
    updateTransitiveClosureOfNonDetectablePrecedence()

  def subscribe(c: Constraint): Unit = {
    associatedPropagators += c
  }

  def nonDetectableSuccessorsOf(index: Int): Int = nonDetectableSuccessors(index).fillArray(nonDetectableSuccessorsContainers(index))
  def nonDetectablePredecessorsOf(index: Int): Int = nonDetectablePredecessors(index).fillArray(nonDetectablePredecessorsContainers(index))
  def isPrecDetectable(from: Int, to: Int) : Boolean = starts(from).max < ends(to).min
  def isPrecNonDetectable(from: Int, to: Int) : Boolean = nonDetectableSuccessors(from).hasValue(to)
  def hasPrec(from: Int, to: Int): Boolean = isPrecDetectable(from, to) || isPrecNonDetectable(from, to)
  def hasPrecForPair(from: Int, to: Int) = hasPrec(from, to) || hasPrec(to, from)
  def hasNonDetectablePrecForPair(from: Int, to: Int) = isPrecNonDetectable(from, to) || isPrecNonDetectable(to, from)

  def addNonDetectablePrecAndUpdateTransitiveClosure(from: Int, to: Int): Unit = {
    if(!nonDetectableSuccessors(from).hasValue(to)) {
      if(nonDetectableSuccessors(to).hasValue(from) || nonDetectablePredecessors(from).hasValue(to) ) {
        throw Inconsistency
      }
      addNonDetectablePrec(from, to)
      updateTransitiveClosureOfNonDetectablePrecedence()
    }
  }

  def triggerPropagation() = {
    var i = 0
    while (i < associatedPropagators.length) {
      associatedPropagators(i).propagate()
      i += 1
    }
    _cp.propagate()
  }

  def topologicalOrder() : Array[Int] = {
    clearInternalStructures()
    fillPostOrder(_nTasks, tasks)
    var t = 0
    while(t < _nTasks) {
      topoOrder(t) = postOrderArray(_nTasks - 1 - t)
      t += 1
    }
    topoOrder
  }

  private def addNonDetectablePrec(from: Int, to: Int): Unit = {
    nonDetectableSuccessors(from).add(to)
    nonDetectablePredecessors(to).add(from)
    tasksInNonDetectablePrecs.add(from)
    tasksInNonDetectablePrecs.add(to)
  }

  private def updateTransitiveClosureOfNonDetectablePrecedence(): Unit = {
    clearSuccessorsInTransitiveClosure()
    //dfs post order is equivalent to a reversed topology sort
    val postOrder = nonDetectableDFSPostOrder()
    //visit the topology sort in reversed order to compute the transitive closure of the DAG of non detectable precedences
    var indexInPostOrder = 0
    var currentNode = postOrder(indexInPostOrder)
    while (currentNode != EMPTY_NODE && indexInPostOrder < _nTasks ){
      var predecessorIndex = 0
      while (predecessorIndex < _nTasks) {
        //for all predecessor of the current node, we add to their successors the successors of the current node
        if(nonDetectableSuccessors(predecessorIndex).hasValue(currentNode)) {
          successorsInTransitiveClosure(predecessorIndex).addAll(successorsInTransitiveClosure(currentNode))
        }
        predecessorIndex += 1
      }

      indexInPostOrder += 1
      if(indexInPostOrder < _nTasks)
        currentNode = postOrder(indexInPostOrder)
    }

    //update the graph with transitive closure
    var t = 0
    while(t < _nTasks) {
      var s = 0
      val nSuccs = successorsInTransitiveClosure(t).fillArray(successorsInTransitiveClosureContainers(t))
      while(s < nSuccs) {
        val succ = successorsInTransitiveClosureContainers(t)(s)
        if(t != succ && !nonDetectableSuccessors(t).hasValue(succ)){ //if new non-detectable precedence
          addNonDetectablePrec(t,succ)
        }

        s += 1
      }
      t += 1
    }
  }

  private def nonDetectableDFSPostOrder() : Array[Int] = {
    clearInternalStructures()
    val nTasksInNonDetectablePrecs = tasksInNonDetectablePrecs.fillArray(tasksInNonDetectablePrecsContainer)
    fillPostOrder(nTasksInNonDetectablePrecs, tasksInNonDetectablePrecsContainer) //reversed dfs post order
    postOrderArray
  }

  private def dagPostOrderDFS(startingNode: Int): Unit = {
    stack.push(startingNode)
    while(!stack.isEmpty) {
      val elem = stack.peek()
      if(nodeStatus(elem) == UNSEEN) {
        val nSuccs : Int = nonDetectableSuccessorsOf(elem)
        var s = 0
        while(s < nSuccs) {
          val succ = nonDetectableSuccessorsContainers(elem)(s)
          if(nodeStatus(succ) == UNSEEN) {
            stack.push(succ)
          }
          s += 1
        }
        nodeStatus(elem) = EXPANDED
      }
      else { // we visited all children of elem, so we can do the postorder action
        if(nodeStatus(elem) == EXPANDED) {
          postOrder.enqueue(elem)
          nodeStatus(elem) = CLOSED
        }
        stack.pop()
      }
    }
  }

  private def clearInternalStructures() = {
    var v = 0
    while(v < _nTasks) {
      nodeStatus(v) = UNSEEN
      postOrderArray(v) = EMPTY_NODE
      v += 1
    }
  }

  private def fillPostOrder(nElem: Int, tasks: Array[Int]) = {
    var v = 0
    while(v < nElem) {
      val t = tasks(v)
      if(nodeStatus(t) == UNSEEN) {
        dagPostOrderDFS(t)
      }
      v += 1
    }
    v = 0
    while(!postOrder.isEmpty) {
      postOrderArray(v) = postOrder.dequeue()
      v += 1
    }
  }

  private def clearSuccessorsInTransitiveClosure() = {
    var v = 0
    while(v < _nTasks) {
      successorsInTransitiveClosure(v).clear()
      successorsInTransitiveClosure(v).add(v)
      v += 1
    }
  }


}

object PrecedenceGraph {
  def apply(starts:Array[CPIntVar], durations:Array[CPIntVar], ends: Array[CPIntVar], ttMatrix: Array[Array[Int]], initialKnownPrecedences: Array[(Int,Int)] = Array()) = {
    new PrecedenceGraph(starts, durations, ends, ttMatrix, initialKnownPrecedences)
  }

  def apply(starts:Array[CPIntVar], durations:Array[CPIntVar], ends: Array[CPIntVar], initialKnownPrecedences: Array[(Int,Int)]) = {
    new PrecedenceGraph(starts, durations, ends, Array.fill(starts.length,starts.length)(0), initialKnownPrecedences)
  }

  def apply(starts:Array[CPIntVar], durations:Array[CPIntVar], ends: Array[CPIntVar]) = {
    new PrecedenceGraph(starts, durations, ends, Array.fill(starts.length,starts.length)(0))
  }
}
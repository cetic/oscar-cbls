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

package oscar.cbls.lib.invariant.graph

import java.io.PrintWriter

import oscar.cbls._
import oscar.cbls.algo.graph._
import oscar.cbls.algo.heap.BinomialHeapWithMove
import oscar.cbls.core.computation.{ChangingSetValue, Invariant, SetNotificationTarget}
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.{SortedMap, SortedSet}

/**
  * Factory for [[oscar.cbls.invariant.graph.KVoronoiZones]] 
  * 
  */

object KVoronoiZones {

  /*****************************************************************************
   * Construct an instance of the KVoronoiZone Invariant
   * 
   * Given
   * - a Graph
   * - a Set of edges that are open on the graph
   * - a set of centroids
   * This invariant computes for each node of the graph the k centroids that are closest to the node and the associated distance.
   * 
   * See [[oscar.cbls.invariant.graph.KVoronoiZones]] for more informations
   * 
   * @param graph The conditional Graph
   * @param openConditions The oscar set variable that contains the edges that are open
   * @param centroids  The oscar set variable that contains the active centroids
   * @param k The number of centroids we want to compute for all the nodes
   * @param trackedNodes The nodes for which we want an output
   * @param m The oscar.cbls store for the variable
   * @param defaultDistanceForUnreachableNode The default distance if no centroid is reachable
   * @param defaultCentroidForUnreachableNode The default centroid if no centroid is reachable
   * 
   * ***/
  def apply(graph: ConditionalGraph,
            openConditions: SetValue,
            centroids: SetValue,
            k: Int,
            trackedNodes: Iterable[Int],
            m: Store,
            defaultDistanceForUnreachableNode: Long,
            defaultCentroidForUnreachableNode: Int = -1
            ) : KVoronoiZones = {

    val trackedNodesMap = SortedMap.empty[Int,Array[(CBLSIntVar,CBLSIntVar)]] ++ trackedNodes.map(
      (nodeId: Int) => {
        nodeId ->
          Array.tabulate(k)((centroidI: Int) => {
            (new CBLSIntVar(m, defaultCentroidForUnreachableNode, Domain(0L, defaultCentroidForUnreachableNode max Long.MaxValue), "closest centroid number " + centroidI + " for node " + nodeId),
              new CBLSIntVar(m, defaultDistanceForUnreachableNode, Domain(0L, defaultDistanceForUnreachableNode max Long.MaxValue), "distance to centroid number " + centroidI + " for node " + nodeId))
          })
      })

    new KVoronoiZones(graph,
      openConditions,
      centroids,
      trackedNodesMap,
      k,
      defaultCentroidForUnreachableNode,
      defaultDistanceForUnreachableNode)


  }
}


/**
  * Given
  * - a Graph
  * - a Set of edges that are open on the graph
  * - a set of centroids
  * This invariant computes for each node of the graph the k centroids that are closest to the node and the associated distance.
  * 
  * 
  * For one node means that:
  * - The first centroid is closer than any other centroid
  * - The second centroid is closer than any other centroid *except* the first one
  * - ...
  * - The kth centroid is closer than any other centroid *except* the (k-1)th, ..., the second one and the first one
  * 
  * 
  * In practical, the k closest centroids are not interesting for all the nodes. It is possible to give the interesting centroids
  *  we are considering through trackedNodeToDistanceAndCentroidMap parameter
  * 
  * 
  * @param graph a graph, this is a constant. it is a conditional graph, so some edges have
  *              a Boolean proposition associated to them
  * @param openConditions the set of condition indexes that are open at a given moment of the search. This is one of the input variables of the constraint
  * @param centroids the set of centroids we are considering at a givent moment of the search. This is one of the input variables of the constraint
  * @param trackedNodeToDistaxnceAndCentroidMap This is the output of the constraint
  *                                               For each node that requires it, the output is a table with k couples
  *                                               The couple in position i \in 1..k contains the ith closest centroid and the distance to the centroid
  * @param defaultCentroidForUnreachbleNode The default value for the variable that contains the centroid if no centroid is reachable
  * @param defaultDistanceForUnReachableNode The default distance for the variable that contains the distance to the centroid if no centroid is reachable
  * 
  * @author tfayolle@cetic.be
  * 
  * **/

class KVoronoiZones(graph:ConditionalGraph,
                    openConditions:SetValue,
                    val centroids:SetValue,
                    val trackedNodeToDistanceAndCentroidMap:SortedMap[Int,Array[(CBLSIntVar,CBLSIntVar)]],
                    k : Int,
                    defaultCentroidForUnreachableNode : Int = -1,
                    defaultDistanceForUnreachableNode : Long)
  extends Invariant with SetNotificationTarget{

  require(k > 0)
  registerStaticAndDynamicDependency(centroids)
  registerStaticAndDynamicDependency(openConditions)
  finishInitialization()



  val trackedNodeToDistanceAndCentroid : Array[Array[(CBLSIntVar,CBLSIntVar)]] =
    Array.tabulate(graph.nbNodes)((nodeID : Int) => {
      trackedNodeToDistanceAndCentroidMap.get(nodeID) match {
        case None => null
        case Some(tab) =>
          tab.map((varCouple: (CBLSIntVar, CBLSIntVar)) => {
            varCouple._1.setDefiningInvariant(this)
            varCouple._2.setDefiningInvariant(this)
          })
          tab
      }
    })

  private def insertInCentroidTab(nodeID : Int,centroidID : Int,distance : Long) : Unit = {
    var centroidToInsert = centroidID
    var distanceToInsert = distance
    var index = 0
    while (index < k) {
      if (trackedNodeToDistanceAndCentroid(nodeID)(index)._1.newValue == centroidToInsert) {
        trackedNodeToDistanceAndCentroid(nodeID)(index)._2 := distanceToInsert
        index = k
      } else {
        if (trackedNodeToDistanceAndCentroid(nodeID)(index)._1.newValue == centroidID) {
          trackedNodeToDistanceAndCentroid(nodeID)(index)._1 := centroidToInsert
          trackedNodeToDistanceAndCentroid(nodeID)(index)._2 := distanceToInsert
          index = k
        } else {
          if (distance < trackedNodeToDistanceAndCentroid(nodeID)(index)._2.newValue || (distance == trackedNodeToDistanceAndCentroid(nodeID)(index)._2.newValue && centroidID < trackedNodeToDistanceAndCentroid(nodeID)(index)._1.newValue)) {
            val savedCentroid = trackedNodeToDistanceAndCentroid(nodeID)(index)._1.newValueInt
            val savedDistance = trackedNodeToDistanceAndCentroid(nodeID)(index)._2.newValue
            trackedNodeToDistanceAndCentroid(nodeID)(index)._1 := centroidToInsert
            trackedNodeToDistanceAndCentroid(nodeID)(index)._2 := distanceToInsert
            centroidToInsert = savedCentroid
            distanceToInsert = savedDistance
          }
          index = index + 1

        }
      }
    }
  }

//  private def insertInCentroidTab2(nodeID : Int,centroidID : Int,distance : Long, i : Int = 0) : Unit = {
//    if (i != k) {
//      if (distance < trackedNodeToDistanceAndCentroid(nodeID)(i)._2.newValue || (distance == trackedNodeToDistanceAndCentroid(nodeID)(i)._2.value && centroidID < trackedNodeToDistanceAndCentroid(nodeID)(i)._1.newValue)) {
//        val nodeToShift = trackedNodeToDistanceAndCentroid(nodeID)(i)._1.newValue
//        val distanceToShift = trackedNodeToDistanceAndCentroid(nodeID)(i)._2.newValue
//        trackedNodeToDistanceAndCentroid(nodeID)(i)._1 := centroidID
//        trackedNodeToDistanceAndCentroid(nodeID)(i)._2 := distance
//        insertInCentroidTab(nodeID,nodeToShift,distanceToShift,i + 1)
//      } else {
//        insertInCentroidTab(nodeID,centroidID,distance,i + 1)
//      }
//    }
//  }

  private def deleteInCentroidTab(nodeID : Int,centroidID : Int) : Unit = {
    //println("Before : " + trackedNodeToDistanceAndCentroid(nodeID).map(e => "(" + e._1.name + ":=" + e._1.newValue + "," + e._2.name + ":=" + e._2.newValue).mkString(";"))
    //println("Delete Centroid " + centroidID + " From " + nodeID)

    var indexTo = 0
    var indexFrom = 0
    while (indexTo < k) {
      //println(i)
      if (trackedNodeToDistanceAndCentroid(nodeID)(indexTo)._1.newValue == centroidID) {
        indexFrom = indexFrom + 1
      }
      if (indexFrom != indexTo){
        if (indexFrom == k){
          trackedNodeToDistanceAndCentroid(nodeID)(indexTo)._1 := defaultCentroidForUnreachableNode
          trackedNodeToDistanceAndCentroid(nodeID)(indexTo)._2 := defaultDistanceForUnreachableNode
        } else {
          trackedNodeToDistanceAndCentroid(nodeID)(indexTo)._1 := trackedNodeToDistanceAndCentroid(nodeID)(indexFrom)._1.newValue
          trackedNodeToDistanceAndCentroid(nodeID)(indexTo)._2 := trackedNodeToDistanceAndCentroid(nodeID)(indexFrom)._2.newValue
        }
      }
      indexFrom += 1
      indexTo += 1
    }
    //println("After : " + trackedNodeToDistanceAndCentroid(nodeID).map(e => "(" + e._1.name + ":=" + e._1.newValue + "," + e._2.name + ":=" + e._2.newValue).mkString(";"))
  }

  case class NodeLabeling(node : Node,centroid : Node,var distance : Long,var positionInHeapMap : Int,var isInHeap : Boolean) {
    override def toString() = "NodeLabeling(" + node.id + "," + centroid.id + "," + distance + "," + position + "," + isInHeap + ")"
  }

  implicit val A : Ordering[NodeLabeling] = new Ordering[NodeLabeling] {
    override def compare(x: NodeLabeling, y: NodeLabeling): Int = {
      if (x.node.id == y.node.id) {
        if (x.centroid.id == y.centroid.id){
          if (x.distance == y.distance && x.isInHeap == y.isInHeap)
            0
          else
            throw new Error("Could not compare a node with same node and centroid - First :" + x + " -- Second : " + y)
        }
        else
          y.centroid.id - x.centroid.id
      } else {
        y.node.id - x.node.id
      }
    }
  }

  case class KClosestCentroidLabeling(node : Node) {
    var nbOfLabeledCentroid = 0
    var centroidMap : Map[Int,NodeLabeling] = Map[Int,NodeLabeling]()
    //var centroidHeap = new BinomialHeapWithMove[NodeLabeling](label => - ((label.distance * centroids.domain.size) + label.centroid.id) ,k)
    var centroidList : List[NodeLabeling] = Nil

    def farthestCentroidIsFarthestThan(distance : Long,centroidID : Long) : Boolean = {
      if (nbOfLabeledCentroid == k)
        distance < centroidList.head.distance || (centroidList.head.distance == distance && centroidID < centroidList.head.centroid.id)
      else
        true
    }

    private def insertLabelInCentroidList(l : NodeLabeling,listOfCentroid : List[NodeLabeling],shift : Long,i : Int = nbOfLabeledCentroid - 1) : List[NodeLabeling] = {
      listOfCentroid match {
        case Nil => l::Nil
        case head::tail =>
          if (head.centroid.id == l.centroid.id)
            insertLabelInCentroidList(l,tail,1,i)
          else {
            if (l.distance < head.distance || (l.distance == head.distance && l.centroid.id < head.centroid.id)) {
              head :: insertLabelInCentroidList(l, tail,shift , i - 1)
            } else {
              l :: head :: tail
            }
          }
      }
    }

    def insertOrCorrect(centroid : Node,distance : Long) : Option[NodeLabeling] = {
      val toReturn =
        centroidMap.get(centroid.id) match {
          case None =>
            if (nbOfLabeledCentroid < k) {
              val label = NodeLabeling(node,centroid,distance,0,false)
              nbOfLabeledCentroid += 1
              centroidList = insertLabelInCentroidList(label,centroidList,1)
              centroidMap = centroidMap + (centroid.id -> label)
              Some(label)
            } else {
              if (farthestCentroidIsFarthestThan(distance, centroid.id)) {
                val label = NodeLabeling(node,centroid,distance,0,false)
                centroidList = centroidList match {
                  case Nil => throw new Error("Node has " + nbOfLabeledCentroid + " labeled but the list is Empty")
                  case h::t =>
                    centroidMap = centroidMap - h.centroid.id
                    if (h.isInHeap)
                      nodeHeapToTreate.delete(h)
                    insertLabelInCentroidList(label,t,1)
                }
                centroidMap = centroidMap + (centroid.id -> label)
                Some(label)
              } else
                None
            }
          case Some(label) =>
            if (distance < label.distance) {
              label.distance = distance
              centroidList = insertLabelInCentroidList(label,centroidList,0)
              Some(label)
            } else
              None
        }
      toReturn
    }

//    def insertOrCorrect(centroid : Node,distance : Long) : Option[NodeLabeling] = {
//      //println(centroidMap.iterator.mkString(";"))
//      var toReturn : Option[NodeLabeling] = None
//      centroidMap.get(centroid.id) match {
//        case None =>
//          if (nbOfLabeledCentroid < k) {
//            val label = NodeLabeling(node,centroid,distance,false)
//            centroidMap = centroidMap + (centroid.id ->label)
//            centroidHeap.insert(label)
//            nbOfLabeledCentroid += 1
//            toReturn = Some(label)
//          } else {
//            if (farthestCentroidIsFarthestThan(distance,centroid.id)) {
//              val lastLabel = centroidHeap.removeFirst()
//              centroidMap = centroidMap - lastLabel.centroid.id
//              val label = NodeLabeling(node,centroid,distance,false)
//              centroidMap = centroidMap + (centroid.id -> label)
//              centroidHeap.insert(label)
//              toReturn = Some(label)
//            } else {
//              toReturn = None
//            }
//          }
//        case Some(element) =>
//          //println("Element : " + element)
//          //if (node.id == 0 && centroid.id == 1)
//            //println(element)
//          if (distance < element.distance) {
//            //println(element)
//            element.distance = distance
//            centroidHeap.notifyChange(element)
//            toReturn = Some(element)
//          } else
//            toReturn = None
//      }
//      toReturn
//    }

    private def removeLabelFromList(label : NodeLabeling, listOfCentroid : List[NodeLabeling]) : List[NodeLabeling] = {
      listOfCentroid match {
        case Nil => throw new Error("Should not happend")
        case head::tail =>
          if (head.centroid.id == label.centroid.id)
            tail
          else {
            head :: removeLabelFromList(label, tail)
          }
      }
    }

    def remove(centroid : Node) : Option[NodeLabeling] = {
      centroidMap.get(centroid.id) match {
        case None => None
        case Some(label) =>
          centroidMap = centroidMap - label.centroid.id
          require(centroidList.contains(label))
          centroidList = removeLabelFromList(label,centroidList)
          nbOfLabeledCentroid = nbOfLabeledCentroid - 1
          Some(label)
      }
    }
  }

  private val nodeHeapToTreateToto = new BinomialHeapWithMove[NodeLabeling](totreate => totreate.distance,graph.nbNodes * centroids.domain.iterator.length)

  class NodeLabelingMap extends scala.collection.mutable.Map[NodeLabeling,Int] {
    def get(k : NodeLabeling):Option[Int] = {
      if (k.positionInHeapMap == -1)
        None
      else
        Some(k.positionInHeapMap)
    }

    def iterator: Iterator[(NodeLabeling, Int)] = {throw new Exception("enumeration not supported"); null}

    def +=(nodeLabelingAndPos : (NodeLabeling,Int)) = {
      nodeLabelingAndPos._1.positionInHeapMap = nodeLabelingAndPos._2
      this
    }

    def -=(nodeLabeling: NodeLabeling) = {
      nodeLabeling.positionInHeapMap = -1
      this
    }
  }

  val nodeLabelingMap : scala.collection.mutable.Map[NodeLabeling,Int]= new NodeLabelingMap

  //private val nodeHeapToTreate = new BinomialHeapWithMoveExtMem[NodeLabeling](totreate => totreate.distance,graph.nbNodes * centroids.domain.size,nodeLabelingMap)

  private val nodeHeapToTreate = new BinomialHeapWithMove[NodeLabeling](totreate => totreate.distance,graph.nbNodes * centroids.domain.sizeInt)

  val nodeLabeling = Array.tabulate(graph.nbNodes)(nodeIndex => KClosestCentroidLabeling(graph.nodes(nodeIndex)))
  centroids.value.foreach(index =>
    {
      tryLabelNode(graph.nodes(index),0,graph.nodes(index))
    })


  //centroids.value.foreach(c => nodeHeapToTreate.insert(NodeLabeling(graph.nodes(c),graph.nodes(c),-1,0,true)))

  var isConditionalEdgeOpen : Array[Boolean] = Array.fill(graph.nbConditions)(false)

  openConditions.value.foreach(e => isConditionalEdgeOpen(e) = true)

  treateNodeInHeap()

  private def insertOrCorrectNodeIntoHeap(label: NodeLabeling,loadNoTransitNode : Boolean = false) = {
    if (label.node.transitAllowed || label.node == label.centroid) {
      if (label.isInHeap) {
        //println(label)
        //println(nodeHeapToTreate.getElements.mkString(";"))
        nodeHeapToTreate.notifyChange(label)
      } else {
        //println(label)
        //println(nodeHeapToTreate.getElements.mkString(";"))
        label.isInHeap = true
        nodeHeapToTreate.insert(label)
      }
    }
  }

//  private def insertNodeInHeapIfNotPresent(label : NodeLabeling) = {
//    if (!label.isInHeap) {
//      label.isInHeap = true
//      nodeHeapToTreate.insert(label)
//    }
//  }

  private def tryLabelNode(node : Node,distance : Long,centroid : Node) {
    //println("Try Labeling : (" + node + "," + centroid + ") with distance " + distance + " --- Currently " + nodeLabeling(node.id).centroidList.mkString(";"))
    val labelOfNode = nodeLabeling(node.id)
    //println(labelOfNode.farthestDistance)
    if (labelOfNode.farthestCentroidIsFarthestThan(distance,centroid.id)) {
      labelOfNode.insertOrCorrect(centroid,distance) match {
        case None =>
        case Some(label) =>
          insertOrCorrectNodeIntoHeap(label)
          //println("Labeling : " + label)
          //println("After : (" + node + "," + centroid + ") with distance " + distance + " --- New " + nodeLabeling(node.id).centroidList.mkString(";"))
          if (trackedNodeToDistanceAndCentroid(node.id) != null) {
            //println(trackedNodeToDistanceAndCentroid(node.id).map(i => i._1.name + ":=" + i._1.newValue + " - " + i._2.name + ":=" + i._2.newValue).mkString(";"))
            //println("Insert in node " + node.id + " - centroid : " + centroid.id + " - distance : " + distance )
            insertInCentroidTab(node.id,centroid.id,distance)
            //println(trackedNodeToDistanceAndCentroid(node.id).map(i => i._1.name + ":=" + i._1.newValue + "-" + i._2.name + ":=" + i._2.newValue).mkString(";"))

          }
      }

    }
  }



  private def isEdgeOpen(e : Edge) = {
    e.conditionID match {
      case None => true
      case Some(condition) => isConditionalEdgeOpen(condition)
    }
  }

  private def treateNodeInHeap(): Unit ={
    //println(Array.tabulate(graph.nbNodes)(i => i + " : " + nodeLabeling(i).centroidHeap.getElements.mkString(";")).mkString("\n"))
    //println("Mark nodes")
    while (!nodeHeapToTreate.isEmpty){
      val totreate = nodeHeapToTreate.removeFirst()
      totreate.isInHeap = false
      val node = totreate.node
      val centroid = totreate.centroid
      val distance = totreate.distance
      //println("Node to Treate : " + totreate)
      //println("To treate Heap : " + nodeHeapToTreate.getElements.mkString(";"))
      //println(node.incidentEdges.map(e => e.otherNode(node).id + " - " +  (distance + e.length).toString).mkString(";"))
      for (edge <- node.incidentEdges){
        if (isEdgeOpen(edge)) {
          //println(node.id + " -> " + edge.otherNode(node).id)
          val otherNode = edge.otherNode(node)
          val otherNodeIndex = otherNode.id
          //println("labelNode(" + node.id + "," + (distance + edge.length).toString + "," + otherNode.id + ")" )
          tryLabelNode(otherNode, distance + edge.length, totreate.centroid)
        }
      }
    }
    //println("End Mark Nodes")
    //println(nodeLabeling.map(n e=> n.node.id + ":" + n.nbOfLabeledCentroid + ":" + n.centroidMap.iterator.map(i => i._2).mkString(";")).mkString("\n"))

  }

  private def removeCentroid(centroid : Node, node : Node) : Option[NodeLabeling] = {
    //println("Node " + node.id + " : Centroids : " + nodeLabeling(node.id).centroidMap.iterator.mkString(";"))
    var toReturn : Option[NodeLabeling] = None
    nodeLabeling(node.id).remove(centroid) match {
      case None => toReturn = None
      case Some(label) =>
        if (label.isInHeap) {
          nodeHeapToTreate.deleteIfPresent(label)
          label.isInHeap = false
        }
        if (trackedNodeToDistanceAndCentroid(node.id) != null) {
          deleteInCentroidTab(node.id,centroid.id)
        }
        toReturn = Some(label)
    }
    //println("Node " + node.id + " : Centroids : " + nodeLabeling(node.id).centroidMap.iterator.mkString(";"))
    toReturn
  }

  private def insertNodeInHeapUntilPosition(listOfLabel: List[NodeLabeling], position: Long): Unit = {
    listOfLabel match {
      case Nil =>
        if(position != 0)
          throw new Error("Nothing to insert in heap")
      case head :: tail =>
        if (position > 0) {
          insertOrCorrectNodeIntoHeap(head)
          insertNodeInHeapUntilPosition(tail, position - 1)
        }
    }
  }


  //case class HoleCreation(node : Node,antecedentPosition : Long)

  private def createHoleAndLoadBoundaryIntoHeap(centroid: Node,node : Node,startingPosition : Long = 0): Unit = {
    //println(node + " - " + centroid)
    val nodeIsInQueue = Array.fill(graph.nbNodes)(false)
    var toTreateNodes = List(node)
    while (toTreateNodes != Nil) {
      val toTreate = toTreateNodes.head
      toTreateNodes = toTreateNodes.tail
      removeCentroid(centroid,toTreate) match {
        case None =>
          nodeLabeling(toTreate.id).centroidList.foreach(l => insertOrCorrectNodeIntoHeap(l))
        case Some(label) =>
          insertNodeInHeapUntilPosition(nodeLabeling(toTreate.id).centroidList, nodeLabeling(toTreate.id).nbOfLabeledCentroid)
          for (edge <- toTreate.incidentEdges) {
            if (isEdgeOpen(edge)) {
              val otherNode = edge.otherNode(toTreate)
              if (!nodeIsInQueue(otherNode.id)) {
                nodeIsInQueue(otherNode.id)
                toTreateNodes = otherNode :: toTreateNodes
              }
            }
          }
      }
    }

  }

  private def loadHedgeExtremitiesIntoHeap(edge: Edge): Unit = {
    val nodeA = edge.nodeA
    val nodeB = edge.nodeB

    nodeLabeling(nodeA.id).centroidList.foreach(l => insertOrCorrectNodeIntoHeap(l))
    nodeLabeling(nodeB.id).centroidList.foreach(l => insertOrCorrectNodeIntoHeap(l))
  }

  private def createHoleFromEdgeExtremities(centroid : Node,node : Node) = {
    var nodeIsInQueue = Array.fill(graph.nbNodes)(false)
    var nodeQueue = List(node)
    nodeIsInQueue(node.id) = true
    while (!nodeQueue.isEmpty) {
      val nodeToTreate = nodeQueue.head
      nodeQueue = nodeQueue.tail

      //println("------------ " + nodeToTreate + " -------------")
      //println("      " + nodeLabeling(nodeToTreate.id).centroidList.mkString(";"))
      removeCentroid(centroid,nodeToTreate) match {
        case None => throw new Error("Only node that have the centroid should be inserted")
        case Some(labelNodeToTreate) =>
          //if (trackedNodeToDistanceAndCentroid(nodeToTreate.id) != null)
            //println(trackedNodeToDistanceAndCentroid(nodeToTreate.id).map(p => p._1).mkString("\n"))
          for (edge <- nodeToTreate.incidentEdges) {
            if (isEdgeOpen(edge)) {
              val otherNode = edge.otherNode(nodeToTreate)
              //println(edge)
              //println(otherNode + " - " + nodeLabeling(otherNode.id).centroidList.mkString(";"))
              nodeLabeling(otherNode.id).centroidMap.get(centroid.id) match {
                case None => nodeLabeling(otherNode.id).centroidList.foreach(l => insertOrCorrectNodeIntoHeap(l))
                case Some(labelOtherNode) =>
                  if (labelOtherNode.distance == labelNodeToTreate.distance + edge.length) {
                    if (!nodeIsInQueue(otherNode.id)) {
                      nodeQueue = otherNode :: nodeQueue
                      nodeIsInQueue(otherNode.id) = true
                    }
                  } else
                    nodeLabeling(otherNode.id).centroidList.foreach(l => insertOrCorrectNodeIntoHeap(l))
              }
            }
          }
      }
      nodeLabeling(nodeToTreate.id).centroidList.foreach(l => insertOrCorrectNodeIntoHeap(l))
    }
  }

  private def createHoleOnEdgeExtremitiesIfNecessary(edge : Edge) : Unit = {
    val nodeA = edge.nodeA
    val nodeB = edge.nodeB
    val edgeLength = edge.length
    //println(edge)
    //println(nodeA.id + " - " + nodeLabeling(nodeA.id).centroidList.mkString(";"))
    //println(nodeB.id + " - " + nodeLabeling(nodeB.id).centroidList.mkString(";"))
    for (labelA <- nodeLabeling(nodeA.id).centroidList) {
      nodeLabeling(nodeB.id).centroidMap.get(labelA.centroid.id) match {
        case None =>
        case Some(labelB) => if (labelB.distance == labelA.distance + edgeLength) createHoleFromEdgeExtremities(labelA.centroid,nodeB)
      }
    }

    //println(nodeHeapToTreate.getElements.mkString(";"))

    for (labelB <- nodeLabeling(nodeB.id).centroidList) {
      nodeLabeling(nodeA.id).centroidMap.get(labelB.centroid.id) match {
        case None =>
        case Some(labelA) => if (labelA.distance == labelB.distance + edgeLength) createHoleFromEdgeExtremities(labelA.centroid,nodeA)
      }
    }

    //println(nodeHeapToTreate.getElements.mkString(";"))
  }
  var i = 0

  override def notifySetChanges(v: ChangingSetValue,
                                id: Int,
                                addedValues: Iterable[Int],
                                removedValues: Iterable[Int],
                                oldValue: SortedSet[Int],
                                newValue: SortedSet[Int]): Unit = {
    val printtikz = false
    i = i + 1
    //println(Array.tabulate(graph.nbNodes)(i => i + " : " + nodeLabeling(i).centroidList.mkString(",")).mkString("\n"))
    //println("Centroids : " + centroids.value.mkString(","))
    //println("Conditions : " + openConditions.value.mkString(","))
    //println("notify")
    if (v == centroids) {
      //println("centroid")

      for (added <- addedValues) {
        //println("added : " + added)
        //TODO  Ajouter les voisins dans le tas pour se couvrir en cas de suppression d'un centroid
        tryLabelNode(graph.nodes(added), 0, graph.nodes(added))
      }

//      println("Centroids : " + centroids.value.mkString(","))
//      println(nodeLabeling(9).centroidList.mkString(";"))
//      println(graph.nodes(9).incidentEdges.map(e => e.otherNode(graph.nodes(9)).id.toString + " : " + nodeLabeling(e.otherNode(graph.nodes(9)).id).centroidList.mkString(";")).mkString("\n"))
      for (removed <- removedValues) {
        //println("removed : " + removed)
        createHoleAndLoadBoundaryIntoHeap(graph.nodes(removed), graph.nodes(removed))
      }
      //println(nodeHeapToTreate.getElements.mkString("\n"))
      ///println(nodeHeapToTreate.getElements.filter(e => e.centroid.id == 4))
      //println(Array.tabulate(graph.nbNodes)(i => i + " : " + nodeLabeling(i).centroidHeap.getElements.mkString(";")).mkString("\n"))

      //println(nodeHeapToTreate.getElements.mkString(";"))



    }
    if (v == openConditions) {
      //println("conditions")

      for (removed <- removedValues) {
        //println("removed : " + removed)
        isConditionalEdgeOpen(removed) = false
        createHoleOnEdgeExtremitiesIfNecessary(graph.conditionToConditionalEdges(removed))
      }

      for (added <- addedValues) {
        //println("added : " + added)
        isConditionalEdgeOpen(added) = true
        loadHedgeExtremitiesIntoHeap(graph.conditionToConditionalEdges(added))
      }
    }
    if (printtikz) {
      val nodes = Array.tabulate(trackedNodeToDistanceAndCentroid.length)(i => {
        if (trackedNodeToDistanceAndCentroid(i) != null) {
          val fstCentroid = trackedNodeToDistanceAndCentroid(i)(0)._1.value
          val sndCentroid = trackedNodeToDistanceAndCentroid(i)(1)._1.value
          "\\node[normalFst" + (if (fstCentroid == -1) "" else ",draw=centroidColor" + fstCentroid + ",fill=centroidColor"+fstCentroid+"!50") + "] at node_" + i + " {}; \n" +
          "\\node[normalSnd" + (if (sndCentroid == -1) "" else ",draw=centroidColor" + sndCentroid + ",fill=centroidColor"+sndCentroid+"!50") + "] at node_" + i + " {};"
        } else
          ""
      }).mkString("\n")
      val tikz = new PrintWriter("nodes" + i + ".tex")
      tikz.write(nodes + "\n\n" + trackedNodeToDistanceAndCentroid.map(p => if (p == null) "" else "%" +  {p.map(i => i._1).mkString(";")}).mkString("\n"))
      tikz.close()
    }

    scheduleForPropagation()
  }

  override def performInvariantPropagation() {
    treateNodeInHeap()
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  override def checkInternals(c: Checker): Unit = {
    val centroidList = centroids.value.toList.map(i => graph.nodes(i))
    //println("Centroids : " + centroidList.mkString(";"))
    def removeCentroidFromList(centroidList : List[Node],centroid : Node) : List[Node] = {
      centroidList match {
        case Nil => Nil
        case head :: tail => if (head == centroid) tail else head::(removeCentroidFromList(tail,centroid))
      }
    }


    for (node <- graph.nodes) {
      var centroidListCopy = centroidList
      val fromScratchNodeLabeling = KClosestCentroidLabeling(node)
      for (i <- 0 until k) {
        //println("Centroids " + centroidListCopy.mkString(";"))
        val fromScratch = new DijkstraMT(graph).search(node, centroidListCopy, (edgeId: Int) => this.openConditions.value.contains(edgeId))
        fromScratch match {
          case Unreachable =>
          case VoronoiZone(c, d,_) =>
            fromScratchNodeLabeling.insertOrCorrect(c, d)
            centroidListCopy = removeCentroidFromList(centroidListCopy, c)
        }
      }
      require(fromScratchNodeLabeling.node == nodeLabeling(node.id).node)
      // println(" -------- Node " + node.id + " -------- ")
      // println("Found Centroids from scratch : " + fromScratchNodeLabeling.centroidMap.iterator.map(e => e._2).mkString(";"))
      // println("Found Centroids iteratif map : " + nodeLabeling(node.id).centroidMap.iterator.map(e => e._2).mkString(";"))
      // println("Found Centroids iteratif List : " + nodeLabeling(node.id).centroidList.mkString(";"))
      if (trackedNodeToDistanceAndCentroid(node.id) != null)
//        println("Tracked Centroid and distance : " + trackedNodeToDistanceAndCentroid(node.id).map(e => e._1.toString + "-" + e._2).mkString(";"))
      require(fromScratchNodeLabeling.nbOfLabeledCentroid == nodeLabeling(node.id).nbOfLabeledCentroid,"node " + node.id + " : Nb of Centroid - From Scratch : " + fromScratchNodeLabeling.nbOfLabeledCentroid + " != Incremental : " + nodeLabeling(node.id).nbOfLabeledCentroid)
      require(nodeLabeling(node.id).centroidList.length == nodeLabeling(node.id).nbOfLabeledCentroid)
      require(nodeLabeling(node.id).centroidMap.iterator.toList.length == nodeLabeling(node.id).nbOfLabeledCentroid,"Node " + node.id + " : Nb of Centroid: " + nodeLabeling(node.id).nbOfLabeledCentroid + " -- Length of centroid list: " + nodeLabeling(node.id).centroidMap.iterator.toList.length)

      def checkCentroidLists(cLIncrementale : List[NodeLabeling],cLFromScratch : List[NodeLabeling], i : Int = 0) : Unit = {
        cLIncrementale match {
          case Nil => require(cLFromScratch == Nil,"centroid list incrementale is empty while centroid list from scratch still has centroids: " + cLFromScratch.mkString(";"))
          case labelInc :: tailInc => require(cLFromScratch != Nil,"centroid list from scratch is empty while centroid list incremental still has centroids: " + cLIncrementale.mkString(";"))
            val labelFS = cLFromScratch.head
            require(labelInc.centroid == labelFS.centroid,"Node " + node.id + " - position " + (fromScratchNodeLabeling.nbOfLabeledCentroid - i - 1) + " - Centroid Incremental: " + labelInc.centroid + " - Centroid From Scratch: " + labelFS.centroid)
            require(labelInc.distance == labelFS.distance,"Node " + node.id + " - position " + (fromScratchNodeLabeling.nbOfLabeledCentroid - i - 1) + " - Centroid Distance Incremental: " + labelInc.distance + " - Centroid Distance From Scratch: " + labelFS.distance)
           if (trackedNodeToDistanceAndCentroid(node.id) != null) {
              val index = nodeLabeling(node.id).nbOfLabeledCentroid - i - 1
              require(trackedNodeToDistanceAndCentroid(node.id)(index)._1.newValue == labelFS.centroid.id,"Node " + node.id + " tracked centroid - incremental : " + trackedNodeToDistanceAndCentroid(node.id)(index)._1.newValue + " != from scratch : " + labelFS.centroid.id)
              require(trackedNodeToDistanceAndCentroid(node.id)(index)._2.newValue == labelFS.distance,"Node " + node.id + " tracked distance - incremental : " + trackedNodeToDistanceAndCentroid(node.id)(index)._2.newValue + " != from scratch : " + labelFS.distance)
            }
            checkCentroidLists(tailInc,cLFromScratch.tail,i + 1)
        }
      }
      checkCentroidLists(nodeLabeling(node.id).centroidList,fromScratchNodeLabeling.centroidList)
      var i = nodeLabeling(node.id).nbOfLabeledCentroid max 0
      while (i < k) {
        if (trackedNodeToDistanceAndCentroid(node.id) != null) {
  //        println(node.id + " : " + i + " : " + trackedNodeToDistanceAndCentroid(node.id)(i)._1)
          require(trackedNodeToDistanceAndCentroid(node.id)(i)._1.value == defaultCentroidForUnreachableNode,"Node " + node.id + " - Centroid " + trackedNodeToDistanceAndCentroid(node.id)(i)._1.value + " should be DefaultCentroidForUnreachableNode : " + defaultCentroidForUnreachableNode)
          require(trackedNodeToDistanceAndCentroid(node.id)(i)._2.value == defaultDistanceForUnreachableNode,"Node " + node.id + " - Distance " + trackedNodeToDistanceAndCentroid(node.id)(i)._2.value + " should be DefaultDistanceForUnreachableNode : " + defaultDistanceForUnreachableNode)
        }
        i = i + 1
      }
    }

    // println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Check Internal OK !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  }

}



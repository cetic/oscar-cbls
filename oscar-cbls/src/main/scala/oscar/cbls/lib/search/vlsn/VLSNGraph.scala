/**
  * *****************************************************************************
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
  * ****************************************************************************
  */

package oscar.cbls.lib.search.vlsn

import oscar.cbls.core.search.{DoNothingMove, Move}
import oscar.cbls._
import scala.collection.immutable.SortedSet

object VLSNMoveType extends Enumeration{
  type VLSNMoveType = Value
  val InsertNoEject, InsertWithEject, MoveNoEject, MoveWithEject, Remove, SymbolicTrashToInsert,SymbolicVehicleToTrash,SymbolicTrashToNodeForEject = Value
}

import oscar.cbls.lib.search.vlsn.VLSNMoveType._

object VLSNSNodeType extends Enumeration {
  type VLSNSNodeType = Value
  val RegularNode, VehicleNode, UnroutedNode, FictiveNode = Value
}

import oscar.cbls.lib.search.vlsn.VLSNSNodeType._


class VLSNNodeBuilder(var nbLabels:Long) {

  var nodes: List[Node] = List.empty
  var nextNodeID: Long = 0L

  def addNode(representedNode:Long, vehicle:Long, label:Long,nodeType:VLSNSNodeType):Node = {
    require(label >=0L && label < nbLabels)
    val n = new Node(nextNodeID, representedNode:Long, nodeType, vehicle:Long, label)
    nextNodeID += 1L
    nodes = n :: nodes
    n
  }

  def newFreshLabel():Long = {
    val toReturn = nbLabels
    nbLabels = nbLabels +1L
    toReturn
  }

  def finish():(Array[Node],Long) = {
    (nodes.reverse.toArray,nbLabels)
  }
}

class VLSNEdgeBuilder(nodes:Array[Node],nbLabels:Long,v:Int){
  val nbNodes = nodes.length
  val edges:Array[Array[Edge]] = Array.tabulate(nbNodes)(_ => Array.fill(nbNodes)(null))
  var fromToWithEdge:List[(Long,Long)] = List.empty
  var nextEdgeID:Long = 0L

  def addEdge(from:Node, to:Node, deltaObj:Long, move:Move, vLSNMoveType: VLSNMoveType):Edge = {
    require(edges(from.nodeID)(to.nodeID) == null)
    val edge = new Edge(from:Node,to:Node, move:Move,deltaObj:Long, nextEdgeID, vLSNMoveType)
    edges(from.nodeID)(to.nodeID) = edge
    nextEdgeID += 1L
    fromToWithEdge = (from.nodeID,to.nodeID) :: fromToWithEdge
    edge
  }

  def finish():VLSNGraph = {
    val edgeArray:Array[Edge] = Array.fill(nextEdgeID)(null)

    for((from,to) <- fromToWithEdge){
      val edge = edges(from)(to)
      edge.registerToNodes()
      edgeArray(edge.edgeID) = edge
    }

    new VLSNGraph(nodes,edgeArray,nbLabels,v)
  }

  override def toString: String = "EdgeBuilder( nbNodes:" + nbNodes + " nbEdges:" + nextEdgeID + ")"
}

/**
  * @param nodes
  * @param edges
  * @param nbLabels labels range from 0L to nbLabels-1L
  */
class VLSNGraph(val nodes:Array[Node],val edges:Array[Edge],val nbLabels:Long, v:Int){
  val nbNodes = nodes.length
  val nbEdges = edges.length

  override def toString: String = "VLSNGraph(nbNodes:" + nbNodes + ",nbEdges:" + nbEdges +
    "\n\tnodes{\n\t\t"+ nodes.mkString("\n\t\t") + "\n\t}" +
    "\n\tedges{\n\t\t" + edges.mkString("\n\t\t") + "\n\t}" + "\n}" + "\n\n\n\n" + toDOT(List(1L,2L,4L).map(edges(_)))

  //"C:\Program Files (x86)\Graphviz2.38\bin\neato" -Tpng  vlsnGraph.dot > a.png
  def toDOT(edgesToBold:List[Edge] = List.empty,light:Boolean = false,onlyCycles:Boolean = false):String = {
    val setOfEdgesToBold = SortedSet.empty[Long] ++ edgesToBold.map(_.edgeID)
    val setOfNodesToBold = SortedSet.empty[Long] ++ edgesToBold.map(_.from.nodeID) ++ edgesToBold.map(_.to.nodeID)
    "##Command to produce the output: \"neato -Tpng thisfile > thisfile.png\"\n" +
      "digraph VLSNGraph {\n" +
      nodes.flatMap(node => {val ofInterest = setOfNodesToBold.contains(node.nodeID)
        if(onlyCycles && !ofInterest) None else Some(node.toDOT(ofInterest))}).mkString("\t", "\n\t", "\n") +
      edges.flatMap(edge => {val ofInterest = setOfEdgesToBold.contains(edge.edgeID)
        if(onlyCycles && !ofInterest) None
        else Some(edge.toDOT(ofInterest,light))}).mkString("\t", "\n\t", "\n") +
      "\tlegend[shape=rectangle,style=filled,fillcolor=pink,color=black,label = \"" + this.statistics + "\"] ; \n" +
      "\toverlap=false\n" +
      //      "\tlabel=\"" + this.statistics + "\";\n" +
      "\tfontsize=12;\n" +
      "}"
  }

  def statistics:String = "VLSNGraph(nbNodes:" + nodes.length + " nbEdges:" + edges.length + ")"
}


class Node(val nodeID:Long, val representedNode:Long, val nodeType:VLSNSNodeType, val vehicle:Long, val label:Long){
  var incoming:List[Edge] = List.empty
  var outgoing:List[Edge] = List.empty

  override def toString: String = "Node(vlsnNode:" + nodeID + " routingNode:" + representedNode + " v:" + vehicle + " label:" + label + ")"

  def toDOT(bold:Boolean):String = {

    val fillColor = nodeType match {
      case RegularNode => "aquamarine"
      case VehicleNode => "green"
      case UnroutedNode => "crimson"
      case FictiveNode => "gold"
    }

    val dotLabel = nodeType match {
      case RegularNode => "\"node:" + representedNode + "\\nvehicle:" + vehicle + "\""
      case VehicleNode => "\"vehicle:" + vehicle + "\""
      case UnroutedNode => "\"node:" + representedNode + "\""
      case FictiveNode => "trashNode"
    }

    val lineColor = (if (bold) "blue" else "black")
    "\"" + nodeID + "\" [shape=circle,style=filled,fillcolor=" + fillColor + ",color=" + lineColor +
      ", label = " + dotLabel + "] ; "
  }
}

class Edge(val from:Node, val to:Node, val move:Move, val deltaObj:Long, val edgeID:Long, val moveType:VLSNMoveType){
  def registerToNodes(): Unit ={
    from.outgoing = this :: from.outgoing
    to.incoming = this :: to.incoming
  }

  override def toString: String = "Edge(from:" + from.nodeID + ",to:"+to.nodeID + ",deltaObj:" + deltaObj + ",type:" + moveType+ ")" + move

  def toDOTHeavy(bold:Boolean = false):String =
    "\"Edge" + edgeID + "\" [shape=rectangle,style=filled,fillcolor=gray, label=\"deltaObj:" + deltaObj +
      "\\n" + moveType +
      "\\n" + (if (move == null) "null" else move.shortString) + "\"" + (if(bold) " color=blue" else "") +"] ; " +
      from.nodeID + " -> " + "\"Edge" + edgeID + "\"" + (if(bold) "[color=blue]" else "") + ";" +
      "\"Edge" + edgeID + "\" -> " + to.nodeID + (if(bold) "[color=blue]" else "") + ";"

  def toDOTLight(bold:Boolean = false):String =
    from.nodeID + " -> " + to.nodeID + (if(bold) "[color=blue]" else "") + ";"

  def toDOT(bold:Boolean = false,light:Boolean):String =
    if(light){ toDOTLight(bold) }else toDOTHeavy(bold)
}


object VLSNGraphTest extends App{

  def buildGraph():VLSNGraph = {
    val nbNodes = 6L
    val nodes = Array.tabulate(nbNodes)(nodeID =>
      new Node(nodeID, nbNodes + nodeID,VLSNSNodeType.RegularNode,nodeID,nodeID))

    val builder = new VLSNEdgeBuilder(nodes: Array[Node], nbNodes,2L) //nbLAbel is set here to nbNodes

    def edge(from: Long, to: Long, gain: Long): Unit = {
      builder.addEdge(nodes(from), nodes(to), gain,  new DoNothingMove(Long.MaxValue),VLSNMoveType.SymbolicTrashToInsert)
    }

    edge(0L, 1L, 10L)
    edge(1L, 2L, -1L)
    edge(2L, 3L, -1L)
    edge(3L, 4L, 1L)
    edge(4L, 0L, 1L)
    edge(0L, 3L, 1L)
    edge(2L, 4L, 1L)
    edge(2L, 5L, -1L)
    edge(5L, 0L, 1L)
    edge(4L, 2L, -1L)
    builder.finish()
  }
  println(buildGraph())
}
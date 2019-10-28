package oscar.cbls.algo.graph

import oscar.cbls.algo.quick.QList

/**
  *
  * @param edges edges. Some of them are conditional, a condition can only appear in one edge,
  *              and all conditions in the declared range must appear in one edge.
  * @param nodes
  * @param nbConditions the number of conditions.
  *                     Conditions are labeled from 0 to nbCondition-1 inclusive.
  *                     all conditions mut appear in one edge.
  */
class ConditionalGraph(val nodes:Array[Node],
                       val edges:Array[Edge],
                       val nbConditions:Int){
  val nbNodes = nodes.length
  val nbEdges = edges.length
  val nodeRange = 0 until nbNodes

  val conditionToConditionalEdges:Array[Edge] = Array.fill[Edge](nbConditions)(null)
  for(edge <- edges){
    edge.conditionID match{
      case None => ;
      case Some(c) =>
        require(conditionToConditionalEdges(c) == null)
        conditionToConditionalEdges(c) = edge
    }
  }
  require(conditionToConditionalEdges.forall(_ != null))

  override def toString: String =
    "ConditionalGraph(nbNodes:" + nbNodes + " nbEdges:" + nbEdges + " nbConditions:" + nbConditions+ "\n\t" +
      "nodes:[\n\t\t" + nodes.mkString("\n\t\t") + "\n\t]" +
      "edges:[\n\t\t" + edges.mkString("\n\t\t") + "\n\t]" + "\n\t)"



  //"C:\Program Files (x86)\Graphviz2.38\bin\neato" -Tpng  vlsnGraph.dot > a.png
  def toDOT:String = {
    "##Command to produce the output: \"neato -Tpng thisfile > thisfile.png\"\n" +
      "graph WiringGraph {\n" +
      nodes.map(node => node.toDOT).mkString("\t", "\n\t", "\n") +
      edges.map(edge => edge.toDOT(this)).mkString("\t", "\n\t", "\n") +
      "\toverlap=false\n" +
      "\tfontsize=12;\n" +
      "}"
  }
}

class ConditionalGraphWithIntegerNodeCoordinates(nodes:Array[Node],
                                                 edges:Array[Edge],
                                                 nbConditions:Int,
                                                 val coordinates:Array[(Int,Int)])
  extends ConditionalGraph(nodes:Array[Node],
    edges:Array[Edge],
    nbConditions:Int){
  require(nodes.length == coordinates.length)
}

class Edge(val id:Int,
           val nodeA:Node,
           val nodeB:Node,
           val length:Long,
           val conditionID:Option[Int]){
  require(length > 0)
  require(nodeA != nodeB)

  val nodeIDA:Int = nodeA.id
  val nodeIDB:Int = nodeB.id

  nodeA.registerEdge(this)
  nodeB.registerEdge(this)

  def otherNode(node:Node):Node = if(node == nodeA) nodeB else nodeA

  def isIncidentTo(node:Node):Boolean = {
    node == nodeA || node == nodeB
  }

  override def toString: String =
    "Edge(id:" + id + " nodeA:" + nodeIDA + " nodeB:" + nodeIDB +
      " length:" + length + (conditionID match{case None => ""  case Some(c) => " condition:" + c}) + ")"

  def toDOT(g:ConditionalGraph):String = {
    conditionID match{
      case None => "n" + nodeIDA + " -- " + "n" + nodeIDB + "[label= \"" + length + "\"];"
      case Some(c) => "n" + nodeIDA + " -- " + "n" + nodeIDB + "[label= \"" + length + "\ncond=" + c + "\" color=red];"
    }
  }
}

class Node(val id:Int, val transitAllowed:Boolean = true){
  var incidentEdges:List[Edge] = Nil
  def registerEdge(edge:Edge) {incidentEdges = edge::incidentEdges}

  override def toString: String = "Node(nodeId:" + id + " transitAllowed:" + transitAllowed + ")"

  def toDOT: String = {
    val borderColor = (if(transitAllowed) "black" else "yellow")
    "n" + id + " [shape=circle,style=filled, fillcolor=yellow, color=" + borderColor + ", label = \"" + id + "\" ] ;"
  }
}



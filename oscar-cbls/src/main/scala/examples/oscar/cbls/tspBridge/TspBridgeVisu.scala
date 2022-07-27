package examples.oscar.cbls.tspBridge

import oscar.cbls.algo.graph.{ConditionalGraph, ConditionalGraphWithIntegerNodeCoordinates, Node, RevisableAStar}
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.visual.graph.SimpleGraphViewer

import java.awt.Color
import scala.collection.immutable.SortedSet

class TspBridgeVisu(graph:ConditionalGraphWithIntegerNodeCoordinates,
                    v:Int,
                    n:Int,
                    underApproximatingDistance:(Int,Int) => Long,
                    freeReturn: Boolean = false)
  extends SimpleGraphViewer(graph,scalable = true){

  def redraw(openBridges:SortedSet[Int], routes:IntSequence): Unit ={
    super.clear(false)

    //all edges, simple made
    drawEdges(graph.edges, 1, Color.black, dashed = true)

    //furthermore, open edges are in Green, cosed edges are in RED
    for(condition <- 0 until graph.nbConditions){
      val conditionalEdge = graph.conditionToConditionalEdges(condition)
      if(openBridges contains condition){
        drawEdge(conditionalEdge, 5,Color.green)
      }else{
        drawEdge(conditionalEdge, 2, Color.pink, dashed = true)
      }
    }

    for(nodeId <- 0 until n){
      val node = graph.nodes(nodeId)
      val color = if(routes contains nodeId) {
        Color.BLUE
      }else {
        Color.RED
      }
      if(node.transitAllowed) {
        drawRoundNode(node, color , radius = 3, toolTip = s"routing$node")
      }else{
        drawCrossNode(node ,color, side = 3, toolTip = s"routing$node")
      }
    }

    //println(routes)

    //path in the routing problem
    var currentExplorer = routes.explorerAtAnyOccurrence(0).get
    while(currentExplorer.next match{
      case None => //return
        if (!freeReturn)
          drawPath(graph.nodes(currentExplorer.value), graph.nodes(v-1), openBridges)
        false
      case Some(expl) if expl.value < v =>
        if (!freeReturn)
          drawPath(graph.nodes(currentExplorer.value), graph.nodes(expl.value - 1), openBridges)
        currentExplorer = expl
        true
      case Some(expl) =>
        drawPath(graph.nodes(currentExplorer.value), graph.nodes(expl.value), openBridges)
        currentExplorer = expl
        true
    }){}

    //underlying graph with small nodes, cross for non-transit nodes
    for(node <- graph.nodes){
      if(node.transitAllowed) {
        drawRoundNode(node, Color.BLACK, 1,toolTip = s"simple$node")
      }else{
        drawCrossNode(node ,Color.BLACK, side = 3,toolTip = s"simple$node")
      }
    }

    //routing nodes

    //start points
    for(vehicle <- 0 until v){
      val node = graph.nodes(vehicle)
      if(node.transitAllowed) {
        drawRoundNode(node, Color.ORANGE, radius = 5, s"startPoint$node")
      }else{
        drawCrossNode(node, Color.ORANGE, side = 5, s"startPoint$node")
      }
    }

    super.repaint()
  }

  private val aStarEngine = new RevisableAStar(graph: ConditionalGraph, underApproximatingDistance)

  def drawPath(fromNode:Node, toNode:Node, openConditions:SortedSet[Int]): Unit ={
    //println(fromNode + " -- " + toNode)
    //println(graph.coordinates(fromNode.id) + " -- " + graph.coordinates(toNode.id))
    drawEdges(aStarEngine.getPath(fromNode,toNode,openConditions).get, 2, Color.BLUE)
  }
}
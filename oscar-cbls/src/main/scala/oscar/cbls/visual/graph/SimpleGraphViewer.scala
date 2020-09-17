package oscar.cbls.visual.graph

import java.awt.Color
import java.awt.geom.Line2D.Double
import java.awt.geom.Rectangle2D

import oscar.cbls.algo.graph._
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualCircle, VisualLine, VisualRectangle, VisualShape}

class SimpleGraphViewer(graph:ConditionalGraphWithIntegerNodeCoordinates)
  extends VisualDrawing(false,false){

  this.setDoubleBuffered(true) //does not work.

  val maxX = graph.coordinates.toList.map(_._1).max
  val maxY = graph.coordinates.toList.map(_._2).max

  var xMultiplier = this.getWidth.toDouble / maxX.toDouble
  var yMultiplier = this.getHeight.toDouble / maxY.toDouble

  override def addShape(shape: VisualShape, repaintAfter: Boolean = true): Unit ={
    super.addShape(shape,false)
  }

  def drawGraph(nodeColor:Color, edgeColor:Color, edgeDashed:Boolean): Unit = {
    for(node <- graph.nodes) drawRoundNode(node:Node, nodeColor, 2)
    drawEdges(graph.edges, width = 1, edgeColor, edgeDashed)
  }

  def drawRoundNode(node:Node,
                    color:Color,
                    radius:Int,
                    toolTip:String = null): Unit ={
    val nodeCoordinates = graph.coordinates(node.id)
    val tempPoint = new VisualCircle(this,
      nodeCoordinates._1 * xMultiplier,
      nodeCoordinates._2 * yMultiplier,
      radius)

    tempPoint.innerCol_$eq(color)
    tempPoint.toolTip = if(toolTip == null) "" + node else toolTip
  }

  def drawSquareNode(node:Node, side:Int, color:Color, toolTip:String = null): Unit ={
    val nodeCoordinates = graph.coordinates(node.id)
    val tempPoint = new VisualRectangle(this, new Rectangle2D.Double(
      nodeCoordinates._1 * xMultiplier - side/2,
      nodeCoordinates._2 * yMultiplier - side/2,
      side,
      side))
    tempPoint.innerCol_$eq(color)
    tempPoint.toolTip = if(toolTip == null) "" + node else toolTip
  }

  def drawCrossNode(node:Node, color:Color, side:Int, toolTip:String = null): Unit ={
    val nodeCoordinates = graph.coordinates(node.id)
    val lineV = new VisualLine(this, new Double(
      nodeCoordinates._1 * xMultiplier,
      nodeCoordinates._2 * yMultiplier + side,
      nodeCoordinates._1 * xMultiplier,
      nodeCoordinates._2 * yMultiplier - side))
    val lineH = new VisualLine(this, new Double(
      nodeCoordinates._1 * xMultiplier + side,
      nodeCoordinates._2 * yMultiplier,
      nodeCoordinates._1 * xMultiplier - side,
      nodeCoordinates._2 * yMultiplier))

    lineV.dashed = false
    lineV.borderWidth = side
    lineV.outerCol = color
    lineH.dashed = false
    lineH.borderWidth = side
    lineH.outerCol = color

    val tempPoint = new VisualCircle(this,
      nodeCoordinates._1 * xMultiplier,
      nodeCoordinates._2 * yMultiplier,
      side)
    tempPoint.border = false
    tempPoint.innerCol_$eq(color)
    tempPoint.toolTip = if(toolTip == null) "" + node else toolTip

  }

  def drawEdges(edges:Iterable[Edge], width:Int, color:Color, dashed:Boolean = false): Unit ={
    for(edge <- edges) drawEdge(edge, width, color, dashed)
  }

  def drawEdge(edge:Edge, width:Int, color:Color, dashed:Boolean = false): Unit = {
    val fromCoord = graph.coordinates(edge.nodeIDA)
    val toCoord = graph.coordinates(edge.nodeB.id)

    val line = new VisualLine(this, new Double(
      fromCoord._1 * xMultiplier,
      fromCoord._2 * yMultiplier,
      toCoord._1 * xMultiplier,
      toCoord._2 * yMultiplier))
    line.dashed = dashed
    line.outerCol = color

    line.borderWidth = width
  }

}

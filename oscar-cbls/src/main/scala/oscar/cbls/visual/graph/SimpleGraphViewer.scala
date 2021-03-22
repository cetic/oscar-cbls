package oscar.cbls.visual.graph

import java.awt.Color
import java.awt.geom.Line2D.Double
import java.awt.geom.Rectangle2D

import oscar.cbls.algo.graph._
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualCircle, VisualLine, VisualRectangle, VisualShape}

class SimpleGraphViewer(graph:ConditionalGraphWithIntegerNodeCoordinates, flipped:Boolean = false, scalable:Boolean = false, margin:Int = 0)
  extends VisualDrawing(flipped,scalable){

  this.setDoubleBuffered(true) //does not work.

  val maxX = graph.coordinates.toList.map(_._1).max
  val maxY = graph.coordinates.toList.map(_._2).max

  def xMultiplier = (this.getWidth.toDouble - (2 * margin)) / maxX.toDouble
  def yMultiplier = (this.getHeight.toDouble - (2 * margin)) / maxY.toDouble

  override def addShape(shape: VisualShape, repaintAfter: Boolean = true): Unit ={
    super.addShape(shape,repaintAfter = false)
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
      margin + (nodeCoordinates._1 * xMultiplier),
      margin + (nodeCoordinates._2 * yMultiplier),
      radius)

    tempPoint.innerCol_$eq(color)
    tempPoint.toolTip = if(toolTip == null) "" + node else toolTip
  }

  def drawSquareNode(node:Node, side:Int, color:Color, toolTip:String = null): Unit ={
    val nodeCoordinates = graph.coordinates(node.id)
    val tempPoint = new VisualRectangle(this, new Rectangle2D.Double(
      margin + (nodeCoordinates._1 * xMultiplier - side/2),
      margin + (nodeCoordinates._2 * yMultiplier - side/2),
      side,
      side))
    tempPoint.innerCol_$eq(color)
    tempPoint.toolTip = if(toolTip == null) "" + node else toolTip
  }

  def drawCrossNode(node:Node, color:Color, side:Int, toolTip:String = null): Unit ={
    val nodeCoordinates = graph.coordinates(node.id)
    val lineV = new VisualLine(this, new Double(
      margin + (nodeCoordinates._1 * xMultiplier),
      margin + (nodeCoordinates._2 * yMultiplier) + side,
      margin + (nodeCoordinates._1 * xMultiplier),
      margin + (nodeCoordinates._2 * yMultiplier) - side))
    val lineH = new VisualLine(this, new Double(
      margin + (nodeCoordinates._1 * xMultiplier) + side,
      margin + (nodeCoordinates._2 * yMultiplier),
      margin + (nodeCoordinates._1 * xMultiplier) - side,
      margin + (nodeCoordinates._2 * yMultiplier)))

    lineV.dashed = false
    lineV.borderWidth = side.toFloat
    lineV.outerCol = color
    lineH.dashed = false
    lineH.borderWidth = side.toFloat
    lineH.outerCol = color

    val tempPoint = new VisualCircle(this,
      margin + (nodeCoordinates._1 * xMultiplier),
      margin + (nodeCoordinates._2 * yMultiplier),
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
      margin + (fromCoord._1 * xMultiplier),
      margin + (fromCoord._2 * yMultiplier),
      margin + (toCoord._1 * xMultiplier),
      margin + (toCoord._2 * yMultiplier)))
    line.dashed = dashed
    line.outerCol = color

    line.borderWidth = width.toFloat
  }

}

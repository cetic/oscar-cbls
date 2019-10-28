package oscar.cbls.visual.routing

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

import java.awt.geom.Line2D.Double
import java.awt.Color
import java.awt.geom.Point2D

import oscar.cbls._
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.util.StopWatch
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualCircle, VisualLine, VisualShape}


/**
  * @author fabian.germeau@student.vinci.be
  */

class BasicRoutingMap(vrp: VRP,
                      nodesPositions: Array[(scala.Double,scala.Double)],
                      colorValues: Array[Color],
                      mapSize: Option[Long],
                      refreshRate: Long,
                      toolTipInfo: Option[Int => Option[() => String]]) extends VisualDrawing(false,false) with StopWatch with RoutingMapTrait {

  private lazy val pixelPositionOfNodes: Array[((scala.Double,scala.Double),Int)] = positionsToPixels()
  private lazy val points:Array[VisualCircle] = buildPoints()
  private lazy val lines:Array[VisualLine] = buildLines()
  private val toolTips:Array[String] = Array.fill(vrp.n)("")
  private val mapSizeNow = mapSize.getOrElse(computeSize(nodesPositions))

  private var lastRefresh = 0L

  override def addShape(shape: VisualShape, repaintAfter: Boolean = true): Unit ={
    super.addShape(shape,false)
  }

  /**
    * This method creates the VisualCircle that will be displayed.
    * They represent depots (circle of size 5.0) and customers (circle of size 1.0)
    * @return
    */
  private def buildPoints() ={
    pixelPositionOfNodes.map(position_node => {
      val position = position_node._1
      val node = position_node._2
      // Setting starting information for each node
      toolTips(node) = generateToolTipInfo(node)

      if(node < vrp.v){
        val tempPoint = new VisualCircle(this,position._1,position._2,5.0){
          override def showToolTip(mousePoint: Point2D) = {
            if (shape.contains(drawing.invertTransform(mousePoint))) {
              drawing.showToolTip(toolTips(node))
            }
          }
        }
        tempPoint.innerCol_$eq(colorValues(node))
        tempPoint.toolTip_$eq(toolTips(node))
        tempPoint
      }
      else{
        val tempPoint = new VisualCircle(this,position._1,position._2,2.0){
          override def showToolTip(mousePoint: Point2D) = {
            if (shape.contains(drawing.invertTransform(mousePoint))) {
              drawing.showToolTip(toolTips(node))
            }
          }
        }
        tempPoint.innerCol_$eq(Color.black)
        tempPoint.toolTip_$eq(toolTips(node))
        tempPoint
      }
    })
  }

  /**
    * This methods build the VisualLine that will be displayed.
    * They represent the road of each node to his successor.
    * If the node has no successor, the destination of this line is the node itself.
    * @return
    */
  private def buildLines(): Array[VisualLine] ={
    pixelPositionOfNodes.map(position_node => {
      val position = position_node._1
      val line = new VisualLine(this,
        new Double(position._1, position._2, position._1, position._2))
      line.borderWidth = 3
      line
    })
  }

  /**
    * This method draw the route of each vehicle depending on the specified refresh rate.
    * It simply modify the VisualLine of each encountered node by changing his destination.
    * This avoid re-creating each component much faster than the previous version.
    *
    * @param force Force the route drawing (last drawing after the end of search for instance)
    */
  def drawRoutes(force: Boolean): Unit ={
    val currentTime = getWatch
    if(force || currentTime - lastRefresh >= refreshRate) {
      val routes = Array.tabulate(vrp.v)(v => vrp.getRouteOfVehicle(v))

      for (r <- 0 until vrp.v) {
        val color = colorValues(r)
        var previousPoint = routes(r).head
        var positionCounter = 1
        for (p <- routes(r).drop(1)) {
          lines(longToInt(previousPoint)).outerCol_$eq(color)
          lines(longToInt(previousPoint)).dest = (points(p).getX, points(p).getY)
          toolTips(p) = generateToolTipInfo(p,r,positionCounter)
          previousPoint = p
          positionCounter += 1
        }
        lines(longToInt(previousPoint)).outerCol_$eq(color)
        lines(longToInt(previousPoint)).dest = (points(r).getX, points(r).getY)
      }

      for(unroutedNode <- vrp.unroutedNodes){
        lines(longToInt(unroutedNode)).outerCol_$eq(Color.black)
        lines(longToInt(unroutedNode)).dest = (points(unroutedNode).getX,points(unroutedNode).getY)
        toolTips(unroutedNode) = generateToolTipInfo(unroutedNode)
      }

      lastRefresh = currentTime
    }
  }

  def clear(): Unit ={
    super.clear()
  }

  private def positionsToPixels(): Array[((scala.Double, scala.Double),Int)] = {
    Array.tabulate(vrp.n)(index => {
      ((nodesPositions(index)._1*getHeight/mapSizeNow, nodesPositions(index)._2*getHeight/mapSizeNow),index)
    })
  }

  private def computeSize(nodesPositions: Array[(scala.Double,scala.Double)]): Long ={
    val distanceBetweenNodes = Array.tabulate(vrp.n)(x =>
      Array.tabulate(vrp.n)(y =>
        List(nodesPositions(x)._1 - nodesPositions(y)._1,nodesPositions(x)._2 - nodesPositions(y)._2))).
      flatten
    val maxDistance = distanceBetweenNodes.flatten.max
    (maxDistance*1.05).toInt
  }

  /**
    * This method aggregate the information attached to each node using basic node information (in this method)
    * and the specialized information wanted by the user (toolTipInfo)
    * @return
    */
  private def generateToolTipInfo(node: Int, vehicle: Int = vrp.n, position: Int = vrp.n): String ={
    val defaultString =
      if(node < vrp.v)
        "Depot of vehicle " + vehicle + "\n"
      else if(vehicle == vrp.n)
        "Unrouted node " + node + "\n"
      else
        "Node " + node + " at the " + position + "th position of the vehicle " + vehicle + "\n"

    defaultString +
      (if(toolTipInfo.isDefined) toolTipInfo.get(node).getOrElse("") else "")
  }
}

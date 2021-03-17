package oscar.cbls.business.routing.model.extensions

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.visu.{RoutingMap, RoutingMapTypes}
import oscar.cbls.visual.{ColorGenerator, SingleFrameWindow}

/**
  * This class is used to display your routing problem on a map.
  * Your routing problem can be displayed on a real map or a simple map
  * (blank frame with dots at there respective position)
  *
  * For an ultimate refresh add this at the end of your search procedure : afterMove(myDisplayObject.drawRoutes())
  *
  * @param vrp The basic vehicle routing problem
  * @param nodePositions A list of node's position. nodePosition(0L) represent the position of the first node
  * @param sizeOfMap The size of your map
  * @param refreshRate The refresh rate (be carefull if the refresh rate is to high you may have greate performance issues
  * @param routingMapType The type of map you want to generate
  * @param toolTipInfo A function node => String used to display some information concerning the node.
  *                    NOTE : Basic information is already given :
  *                        "Node " + node + " at the " + position + "th position of the vehicle " + vehicle + "\n"
  * @return A display object
  */

//TODO move this to proper location: business.routing.visu
class Display(vrp: VRP,
              nodePositions: Array[(Double,Double)],
              sizeOfMap: Option[Int] = None,
              refreshRate: Int = 100,
              toolTipInfo: Option[Int => Option[() => String]] = None,
              routingMapType: RoutingMapTypes.Value = RoutingMapTypes.BasicRoutingMap,
              title:String = "VRP with OscaR.cbls"
             ) {

  val routingMap = RoutingMap(vrp,nodePositions, ColorGenerator.generateRandomColors(vrp.v), sizeOfMap, refreshRate,toolTipInfo,routingMapType)
  SingleFrameWindow.show(routingMap, title)

  def drawRoutes(force:Boolean = false): Unit ={
    routingMap.drawRoutes(force)
  }

}

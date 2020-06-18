package oscar.cbls.business.routing.modeling

import oscar.cbls.business.routing._
import oscar.cbls.business.routing.visu.RoutingMapTypes

/**
  * Created by fg on 9/10/17.
  */
//TODO: use proper name here
trait RoutingExtensions {

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
    *                    WARNING : Using html syntax => breakline = < br >
    *                    NOTE : Basic information is already given :
    *                        "Node " + node + " at the " + position + "th position of the vehicle " + vehicle + "< br >"
    * @return A display object
    */
  def display(vrp: VRP,
              nodePositions: Array[(Double,Double)],
              sizeOfMap: Option[Int] = None,
              refreshRate: Int = 100,
              routingMapType: RoutingMapTypes.Value = RoutingMapTypes.BasicRoutingMap,
              toolTipInfo: Option[Int => Option[() => String]] = None,
              title:String = "VRP with OscaR.cbls"
             ) =
    new Display(vrp,nodePositions,sizeOfMap,refreshRate,toolTipInfo, routingMapType,title)
  type Display = oscar.cbls.business.routing.model.extensions.Display

}

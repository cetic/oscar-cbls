package oscar.cbls.business.routing.visu

import java.awt.Color

import javax.swing.JPanel
import oscar.cbls.business.routing.model.VRP

trait RoutingMapTrait{
  def drawRoutes(force: Boolean): Unit
}

object RoutingMapTypes extends Enumeration{
  val BasicRoutingMap, RealRoutingMap = Value
}

object RoutingMap{

  def apply(vrp: VRP,
            nodesPositions: Array[(scala.Double,scala.Double)],
            vehiclesToColor: Array[Color],
            size: Option[Int],
            resfreshRate: Int,
            toolTipInfo: Option[Int => Option[() => String]],
            routingMapType: RoutingMapTypes.Value): JPanel with RoutingMapTrait ={
    routingMapType match{
      case RoutingMapTypes.BasicRoutingMap =>
        new BasicRoutingMap(vrp,nodesPositions,vehiclesToColor,size,resfreshRate,toolTipInfo)
      case RoutingMapTypes.RealRoutingMap =>
        new RealRoutingMap(vrp,nodesPositions,vehiclesToColor,resfreshRate,toolTipInfo)
    }
  }
}

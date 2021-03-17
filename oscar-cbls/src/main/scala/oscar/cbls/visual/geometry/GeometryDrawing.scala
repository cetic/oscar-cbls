package oscar.cbls.visual.geometry

import java.awt.Color

import javax.swing.JPanel
import org.locationtech.jts.geom.Geometry

//TODO: c'est vraiment trop compliqué

trait GeometryDrawingTrait{
  def drawShapes(boundingBoxOn:Option[Geometry] = None,
                 shapes:List[(Geometry,Option[Color],Option[Color],String)],
                 centers:List[(Long,Long)],
                 saveShapesAndPositions: Boolean = true): Unit
}

object GeometryDrawingTypes extends Enumeration{
  val Simple, OnRealMap = Value
}

//TODO: plutôt donner une règle de conversion en option
//conversionDataForRealMapDisplay(originPoint:(Double,Double), )
object GeometryDrawing {
  def apply(relevantDistances: List[(Int,Int)],
            geometryDrawingType: GeometryDrawingTypes.Value = GeometryDrawingTypes.Simple,
            area: Option[List[(Int,Int)]] = None,
            pointOfOrigin: Option[(Double, Double)] = None,
            pointShift: Option[() => (Double,Double)] = None): JPanel with GeometryDrawingTrait ={
    geometryDrawingType match{
      case GeometryDrawingTypes.Simple=>
        new SimpleGeometryDrawing(relevantDistances, pointShift = pointShift)
      case GeometryDrawingTypes.OnRealMap =>
        require(pointOfOrigin.isDefined && area.isDefined,
          "In order to display Geometry drawing on a real map, " +
            "you need to specify the point of origin (lat,lon) and the area of" +
            " the drawing (in cm from origin)")
        new GeometryDrawingOnRealMap(pointOfOrigin.get,area.get)
    }
  }
}

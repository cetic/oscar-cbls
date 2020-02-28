package oscar.cbls.visual.geometry

import java.awt.Color

import org.jdesktop.swingx.mapviewer.{DefaultTileFactory, GeoPosition, TileFactoryInfo}
import org.locationtech.jts.geom.Geometry
import oscar.visual.map.VisualMap

class GeometryDrawingOnRealMap(pointOfOrigin: (Double,Double),
                               area: List[(Int, Int)],
                               windowWidth: Int = 960,
                               windowHeight: Int = 960)
  extends VisualMap()
    with GeometryDrawingTrait {

  val areaGeoCoords: List[GeoPosition] = area.map(p => convertDistToGeoCoords(p._1, p._2))

  val maxZoom = 20
  val geometryDrawingInfo: TileFactoryInfo = new TileFactoryInfo(1, maxZoom-1, maxZoom,
    256, true, true, // tile size is 256 and x/y orientation is normal
    "http://tile.openstreetmap.org",
    "x", "y", "z") {
    override def getTileUrl(x: Int, y: Int, zoo: Int): String = {
      val zoom = maxZoom - zoo
      this.baseURL + "/" + zoom + "/" + x + "/" + y + ".png"
    }
  }

  /**
    * Define the center of map base on the area geographic coordinates
    * @return
    */
  def centerOfMap: GeoPosition = {
    val (minLatitude,maxLatitude,minLongitude,maxLongitude) =
      (areaGeoCoords.map(c => c.getLatitude).min,
        areaGeoCoords.map(c => c.getLatitude).max,
        areaGeoCoords.map(c => c.getLongitude).min,
        areaGeoCoords.map(c => c.getLongitude).max)
    new GeoPosition(
      (maxLatitude + minLatitude)/2,
      (maxLongitude + minLongitude)/2)
  }

  /**
    * Define the top left geographic coordinate.
    * It's needed to adjust the overlapping simple geometry drawing
    */
  lazy val topLeftGeoPosition: GeoPosition = {
    val (maxLatitude,minLongitude) =
      (areaGeoCoords.map(c => c.getLatitude).max,
        areaGeoCoords.map(c => c.getLongitude).min)
      new GeoPosition(maxLatitude,minLongitude)
  }

  val tfRouting = new DefaultTileFactory(geometryDrawingInfo)
  viewer.setTileFactory(tfRouting)
  viewer.setZoom(defineInitialZoom)
  viewer.setAddressLocation(centerOfMap)
  viewer.setName("Geometry Drawing")
  viewer.setPreferredSize(new java.awt.Dimension(windowWidth, windowHeight))
  this.setOpaque(true)

  def topLeftPointShiftInPixel: (Double,Double) ={
    val topLeftPointInPixel = viewer.convertGeoPositionToPoint(topLeftGeoPosition)
    (topLeftPointInPixel.getX, topLeftPointInPixel.getY)
  }

  /**
    * This method compute the initial zoom needed to fit the are location
    * @return the initial zoom level
    */
  private def defineInitialZoom: Int ={
    val minLatGeoCoord = areaGeoCoords.minBy(_.getLatitude)
    val maxLatGeoCoord = areaGeoCoords.maxBy(_.getLatitude)
    val minLongGeoCoord = areaGeoCoords.minBy(_.getLongitude)
    val maxLongGeoCoord = areaGeoCoords.maxBy(_.getLongitude)
    val farthestAwayGeoCoords =
      if(tfRouting.geoToPixel(maxLatGeoCoord,1).getY - tfRouting.geoToPixel(minLatGeoCoord,1).getY >
        tfRouting.geoToPixel(maxLongGeoCoord,1).getX - tfRouting.geoToPixel(minLongGeoCoord,1).getX)
        List(new GeoPosition(minLatGeoCoord.getLatitude,minLatGeoCoord.getLongitude),
          new GeoPosition(maxLatGeoCoord.getLatitude,maxLatGeoCoord.getLongitude))
      else
        List(new GeoPosition(minLongGeoCoord.getLatitude,minLongGeoCoord.getLongitude),
          new GeoPosition(maxLongGeoCoord.getLatitude,maxLongGeoCoord.getLongitude))

    def isZoomAdjusted(zoom: Int): Boolean ={
      // Convert to pixle position in whole map with the specified zoom level
      val pixelPosAtZoom = farthestAwayGeoCoords.map(g =>  tfRouting.geoToPixel(g,zoom))
      val xs = pixelPosAtZoom.map(_.getX)
      val ys = pixelPosAtZoom.map(_.getY)
      val maxDistAtZoom = Math.max(xs.max - xs.min,ys.max - ys.min)

      // We want the structure to be clearly visible
      maxDistAtZoom*1.1 < windowWidth
    }
    var zoom = 1
    while(!isZoomAdjusted(zoom))
      zoom += 1
    zoom
  }

  private def convertDistToGeoCoords(xFromOrigin: Long, yFromOrigin: Long): GeoPosition ={
    val oneLatDegreKM = 111.19
    val R = 111.321

    val lat = pointOfOrigin._1 - (yFromOrigin.toDouble/(oneLatDegreKM*100000))
    val long = pointOfOrigin._2 + (xFromOrigin.toDouble/(R*Math.cos(Math.toRadians(lat+pointOfOrigin._1)/2)*100000))
    new GeoPosition(lat, long)
  }

  def drawShapes(boundingBoxOn:Option[Geometry] = None,
                 shapes:List[(Geometry,Option[Color],Option[Color],String)],
                 centers:List[(Long,Long)],
                 saveShapesAndPositions: Boolean = true): Unit ={
  }
}


package oscar.cbls.visual.routing

import com.gluonhq.maps.{MapLayer, MapPoint, MapView}
import oscar.cbls.visual.generator.ColorGenerator
import oscar.cbls.visual.logging.LoggerDisplay
import oscar.cbls.visual.objective.ObjectiveFunctionDisplay
import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.Tooltip
import scalafx.scene.layout.Region
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Polyline}
import scalafx.scene.{Node, Scene}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.math.pow

/**
 * This class sets up a ScalaFX interface for a routing problem
 * @param v number of vehicles
 * @param n number of stop points
 * @param solverProcedure a procedure that computes a routing solution
 */
class RoutingInterface(v: Int, n: Int, solverProcedure: => Unit) extends JFXApp3 {
  /**
   * Private class for the layer of the OSM Map
   */
  private class RoutingMapLayer extends MapLayer {
    private val routeLines = Array.tabulate(v) { i =>
      val line = Polyline()
      line.setStroke(colors(i))
      line.setFill(Color.Transparent)
      line.strokeWidth = 2
      this.getChildren.add(line)
      line
    }

    /**
     * Draws the lines of routes in the map layer
     * @param routes the routes to be drawn
     */
    def drawLines(routes: Array[List[Int]]): Unit = {
      for {i <- routes.indices} {
        routeLines(i).getPoints.clear()
        // First point : the point at index i (route start)
        val startPoint = points(i)._1
        val mapPoint = getMapPoint(startPoint.getLatitude, startPoint.getLongitude)
        routeLines(i).getPoints.addAll(mapPoint.getX, mapPoint.getY)
        // next points : in the route list
        for {p <- routes(i)} {
          val stopPoint = points(p)._1
          val stopMapPoint = getMapPoint(stopPoint.getLatitude, stopPoint.getLongitude)
          routeLines(i).getPoints.addAll(stopMapPoint.getX, stopMapPoint.getY)
        }
      }
      this.markDirty()
    }

    /**
     * Adds a map point representing a point of the routing problem
     * @param p the map point
     * @param node the icon associated to the map point
     */
    def addPoint(p: MapPoint, node: Node): Unit = {
      points.add((p, node))
      this.getChildren.add(node)
      this.markDirty()
    }

    /**
     * Gets the minimum height and width covering the current points
     * @return a tuple (minWidth, minHeight)
     */
    def getMinWidthHeight: (Double, Double) = {
      // Min-Max Points in pixels
      var minXs = Double.MaxValue
      var maxXs = Double.MinValue
      var minYs = Double.MaxValue
      var maxYs = Double.MinValue
      for {candidate <- points} {
        // Point in geocoordinates
        val point = candidate._1
        val pointLat = point.getLatitude
        val pointLon = point.getLongitude
        // Point in pixels
        val mapPoint = getMapPoint(pointLat, pointLon)
        val pointX = mapPoint.getX
        val pointY = mapPoint.getY
        // Update Mins-Maxs
        minXs = minXs min pointX
        maxXs = maxXs max pointX
        minYs = minYs min pointY
        maxYs = maxYs max pointY
      }
      (maxXs-minXs, maxYs-minYs)
    }

    /**
     * Computes the level of zoom and the coordinates of the center of the
     * current points
     * @return a tuple (level of Zoom, Center geocoordinates (lat,lon))
     */
    def zoomCenterPoints(): (Int, (Double, Double)) = {
      // Min-Max Points in pixels
      var minXs = Double.MaxValue
      var maxXs = Double.MinValue
      var minYs = Double.MaxValue
      var maxYs = Double.MinValue
      // Min-Max points in coordinates
      var minLat = Double.MaxValue
      var maxLat = Double.MinValue
      var minLon = Double.MaxValue
      var maxLon = Double.MinValue
      // Main loop
      for {candidate <- points} {
        // Point in geoccordinates
        val point = candidate._1
        val pointLat = point.getLatitude
        val pointLon = point.getLongitude
        // Point in pixels
        val mapPoint = getMapPoint(pointLat, pointLon)
        val pointX = mapPoint.getX
        val pointY = mapPoint.getY
        // Update Mins-Maxs
        minXs = minXs min pointX
        maxXs = maxXs max pointX
        minYs = minYs min pointY
        maxYs = maxYs max pointY
        minLat = minLat min pointLat
        maxLat = maxLat max pointLat
        minLon = minLon min pointLon
        maxLon = maxLon max pointLon
      }
      // Compute zoom
      val maxDist = (maxXs - minXs) max (maxYs - minYs)
      var zoom = 0
      while (maxDist/pow(2, zoom) > 1e-6) {
        zoom += 1
      }
      // Compute center of map
      val latMiddle = (maxLat + minLat)/2
      val lonMiddle = (maxLon + minLon)/2
      // Return
      (zoom + 1, (latMiddle, lonMiddle))
    }

    /**
     * This method is called when the map is redrawn (e.g. after a zoom) and the points (and
     * other elements in the map) must be redrawn accordingly
     */
    override def layoutLayer(): Unit = {
      for {candidate <- points} {
        val point = candidate._1
        val node = candidate._2
        val mapPoint = getMapPoint(point.getLatitude, point.getLongitude)
        node.setVisible(true)
        node.setTranslateX(mapPoint.getX)
        node.setTranslateY(mapPoint.getY)
      }
      for {i <- routes.value.indices} {
        routeLines(i).getPoints.clear()
        // First point : the point at index i (route start)
        val startPoint = points(i)._1
        val mapPoint = getMapPoint(startPoint.getLatitude, startPoint.getLongitude)
        routeLines(i).getPoints.addAll(mapPoint.getX, mapPoint.getY)
        // next points : in the route list
        for {p <- routes.value(i)} {
          val stopPoint = points(p)._1
          val stopMapPoint = getMapPoint(stopPoint.getLatitude, stopPoint.getLongitude)
          routeLines(i).getPoints.addAll(stopMapPoint.getX, stopMapPoint.getY)
        }
      }
    }
  }

  // Interface variables
  private val startTime = System.nanoTime()
  private val routes = ObjectProperty[Array[List[Int]]](Array[List[Int]]())
  private val points = ObservableBuffer[(MapPoint, Node)]()
  private val colors = ColorGenerator.generateRandomColors(v)
  private var mapView: MapView = _
  private var mapLayer: RoutingMapLayer = _
  private var objDisplay: ObjectiveFunctionDisplay = _
  private var logDisplay: LoggerDisplay = _

  // Change interface objects when the routes observable changes
  routes.onChange {
    Platform.runLater {
      mapLayer.drawLines(routes.value)
    }
  }

  /**
   * Sets the points of the routing problem into the map. The first v points are the
   * vehicle starting points. the remaining points are the stopping points
   * @param coords an array of the geocoordinates (lat, lon) for each point
   */
  def setCoords(coords: Array[(Double, Double)]): Unit = {
    Platform.runLater {
      var i = 0
      coords.foreach {
        case (lat, lon) =>
          val mpt = new MapPoint(lat, lon)
          val icon = if (i<v) Circle(4, Color.OrangeRed) else Circle(4, Color.Blue)
          val tooltip = new Tooltip(s"${if (i<v) "Depot point" else "Passage point"} at (${mpt.getLatitude}, ${mpt.getLongitude})")
          Tooltip.install(icon, tooltip)
          mapLayer.addPoint(mpt, icon)
          i += 1
      }
      val (zoom, middleP) = mapLayer.zoomCenterPoints()
      mapView.setZoom(zoom)
      mapView.setCenter(new MapPoint(middleP._1, middleP._2))
      val (mapWidth, mapHeight) = mapLayer.getMinWidthHeight
      stage.width = mapWidth+100
      stage.height = mapHeight+100
    }
  }

  /**
   * Updates the observable value of the routes
   * @param newRoutes the new routes
   */
  def updateRoutes(newRoutes: Array[List[Int]]): Unit = {
    routes.update(newRoutes)
  }

  /**
   * Updates the observable value of objective function
   * @param newObjVal the new value of objective function
   */
  def updateObj(newObjVal: Long): Unit = {
    val newTime = (System.nanoTime() - startTime) * 1e-9
    objDisplay.updateObj(newTime, newObjVal)
    Platform.runLater {
      logDisplay.addLog(s"$newTime s", "Move Found", s"$newObjVal")
    }
  }

  /**
   * Main thread of the ScalaFX routing interface
   */
  override def start(): Unit = {
    mapView = new MapView
    mapLayer = new RoutingMapLayer
    mapView.addLayer(mapLayer)
    mapView.setCenter(new MapPoint(50.640139, 4.666642))
    mapView.setZoom(1)
    val mapRegion = new Region(mapView)
    objDisplay = new ObjectiveFunctionDisplay()
    logDisplay = new LoggerDisplay()
    stage = new JFXApp3.PrimaryStage {
      title = "Routing Interface Example"
      scene = new Scene(800, 600) {
        root = mapRegion
      }
    }
    val objFuncStage = objDisplay.stage
    val logsStage = logDisplay.stage
    objFuncStage.show()
    logsStage.show()
    Future(solverProcedure)
  }
}

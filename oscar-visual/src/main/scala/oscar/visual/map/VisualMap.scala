/*******************************************************************************
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
 ******************************************************************************/
package oscar.visual.map
import java.awt.BorderLayout
import java.awt.Color
import java.io.IOException
import javax.swing.JPanel
import org.jdesktop.swingx.JXMapViewer
import org.jdesktop.swingx.mapviewer.DefaultTileFactory
import org.jdesktop.swingx.mapviewer.GeoPosition
import org.jdesktop.swingx.mapviewer.TileFactoryInfo
import java.awt.Toolkit
import oscar.visual.VisualFrame

/**
 * @author Pierre Schaus
 */
class VisualMap(lineStyle: String = "line") extends JPanel(new BorderLayout()) {

  val viewer = new JXMapViewer()
  var lines = List[MapLine]()
  var waypoints = List[MapWaypoint]()
  var paths = List[MapPath]()
  var polygons = List[MapPolygon]()

  val max = 17
  val info = new TileFactoryInfo(1, max - 2, max,
    256, true, true, // tile size is 256 and x/y orientation is normal
    "http://tile.openstreetmap.org",
    "x", "y", "z") {
    override def getTileUrl(x: Int, y: Int, zoo: Int): String = {
      val zoom = max - zoo
      this.baseURL + "/" + zoom + "/" + x + "/" + y + ".png"
    }

  }

  System.setProperty("http.agent", "Mozilla/5.0")

  val tf = new DefaultTileFactory(info)
  viewer.setTileFactory(tf)
  viewer.setZoom(11)
  viewer.setAddressLocation(new GeoPosition(51.5, 0))

  viewer.setOverlayPainter(new MapPainter(this, lineStyle))

  viewer.setName("mapKit")
  val screensize = Toolkit.getDefaultToolkit.getScreenSize
  viewer.setPreferredSize(new java.awt.Dimension(screensize.width / 2, screensize.height / 2))
  //viewer.setDefaultProvider(DefaultProviders.OpenStreetMaps);
  add(viewer)

  def createWaypoint(lt: Double, lg: Double, col : Color = Color.BLUE, lbl : String = null): MapWaypoint = {
    val res = new MapWaypoint(this, lt, lg, col, lbl)
    waypoints = waypoints :+ res
    refresh()
    res
  }

  def removeWaypoint(wp : MapWaypoint): Unit = {
    waypoints = waypoints.filterNot(_ == wp)
    refresh()
  }

  def createLine(o: (Double, Double), d: (Double, Double), col : Color = Color.RED ): MapLine = {
    val l = new MapLine(this, o._1, o._2, d._1, d._2, col)
    lines = lines :+ l

    refresh()

    l
  }


  def removeLine(line: MapLine): Unit = {
    lines = lines.filterNot(_ == line)
    refresh()
  }

  def clear(): Unit = {
    lines = List[MapLine]()
    waypoints = List[MapWaypoint]()
    paths = List[MapPath]()
    refresh()
  }

  def createPath(o: (Double, Double), d: (Double, Double), col : Color = Color.BLACK): MapPath = {
    val p = new MapPath(this, o._1, o._2, d._1, d._2, col)
    paths = paths :+ p

    refresh()

    p
  }

  def removePath(p: MapPath): Unit = {
    paths = paths.filterNot(_ == p)
    refresh()
  }

  def createPolygon(coords : Array[(Double,Double)], col : Color = Color.GREEN): Unit = {
    val poly = new MapPolygon(this,coords,col)
    polygons = polygons :+ poly
    refresh()
  }

  def removePolygon(poly : MapPolygon): Unit = {
    polygons = polygons.filterNot(_ == poly)
    refresh()
  }

  def refresh(): Unit = {
    viewer.repaint()
  }

}

object VisualMap {
  def main(args: Array[String]): Unit = {

    val f = VisualFrame("toto")
    val inf = f.createFrame("tmap")

    val map = new VisualMap()

    inf.add(map)
    f.pack()

    try {
      /*
      val countries = List("Albania", "Andorra", "Austria", "Belgium", "Bulgaria", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Ireland", "Latvia", "Luxembourg", "Portugal", "Spain", "United Kingdom")
      for (c <- countries) {

        val loc = Geocoder.getLocation(c)
        println(c + " " + loc.lat + "," + loc.lon)
        map.createWaypoint(loc.lat, loc.lon);
        try {
          Thread.sleep(200)
        } catch {
          case e: InterruptedException => e.printStackTrace()
        }

      }

      val ny = Geocoder.getLocation("New York")
      val be = Geocoder.getLocation("Brussels")

      */
      val l = map.createLine((50.294603, 4.800819), (50.844972, 4.357246))
      l.color = Color.blue

      //val pa = map.createPath((50.294603, 4.800819), (50.844972, 4.357246))

    } catch {
      case e1: IOException => e1.printStackTrace()
    }

    map.refresh()

    System.out.println(map.hashCode())
    /*
		try {
			Thread.sleep(2000);
		} catch {
		case e: InterruptedException => e.printStackTrace()
		}
		*/

  }
}


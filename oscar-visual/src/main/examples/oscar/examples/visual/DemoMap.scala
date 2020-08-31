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
package oscar.examples.visual

import oscar.visual.map.VisualMap
import oscar.visual.map.MapWaypoint
import oscar.visual.VisualFrame
import java.io.IOException
import java.awt.Toolkit

import oscar.visual.map.MapPath
import java.awt.Color

import javax.swing.JInternalFrame
import org.jdesktop.swingx.mapviewer.Waypoint

object DemoMap {
  //coordinates
  val bruxelles = (50.847573, 4.34214)
  val paris = (48.844836, 2.339882)
  val london = (51.51558,-0.14447)
  val rome = (41.8941,12.478821)
  val madrid = (40.388397,-3.726013)
  val luxembourg = (49.61071,6.107574)
  val berlin = (52.549636,13.313782)
  val hamsterdam = (52.362183,4.843323)
  
  val capitals = List(bruxelles, paris, london, rome, madrid, luxembourg, berlin, hamsterdam)
  
  var bp: MapPath = _
  var waypoints: List[MapWaypoint] = List[MapWaypoint]()
  
  //display components
  
  val f: VisualFrame = VisualFrame("Map visualization demo")
  val m = new VisualMap()
  val inf: JInternalFrame = f.createFrame("VisualMap")
  
  inf.add(m)

  f.pack()

  def runInThread(p: => Unit): Unit = {
    val thread = new Thread(() => p)
    thread.start()
  }

  def main(args: Array[String]): Unit = {
    val tb = f.createToolBar()

    tb.addButton("routes from capitals to bxl", { runInThread(demoMap()) })
    tb.addButton("add Bruxelles-Paris", { runInThread(bxlparis()) })
    tb.addButton("remove Bruxelles-Paris", { runInThread(removebxlparis()) })
    tb.addButton("mark capitals", { runInThread(wp()) })
    tb.addButton("unmark capitals", { runInThread(removewp()) })
    tb.addButton("clear", { runInThread(clear()) })
    f.pack()
    val screensize = Toolkit.getDefaultToolkit.getScreenSize
    f.setSize(new java.awt.Dimension(screensize.width - 100, screensize.height - 200))
    inf.setSize(new java.awt.Dimension(screensize.width - 150, screensize.height - 300))
  }
  
  /*
   * add a green bruxelles to paris path
   */
  def bxlparis(): Unit = {
    bp = m.createPath(bruxelles, paris, new Color(0, 100, 0))
  }
  
  def removebxlparis(): Unit = {
    m.removePath(bp)
  }
  
  /*
   * put waypoints on capitals
   */
  def wp(): Unit = {
    waypoints = capitals.map(p => m.createWaypoint(p._1, p._2))
  }
  
  def removewp(): Unit = {
    waypoints.foreach(m.removeWaypoint)
    waypoints = List[MapWaypoint]()
  }
  
  /*
   * remove every element from the map
   */
  def clear(): Unit = {
    m.clear()
  }
  
  /*
   * scripted demonstration
   */
  def demoMap(): Unit = {

    try {

      val l = m.createLine(capitals.head, capitals(2))

      capitals.zipWithIndex.filter(p => p._2 != 0 && p._2 != 2).map(_._1).foreach(lo => m.createPath(lo, capitals.head))

    } catch {
      case e1: IOException => e1.printStackTrace()
    }

    try {
      Thread.sleep(2000)
    } catch {
      case e: InterruptedException => e.printStackTrace()
    }

    m.refresh()
  }
}

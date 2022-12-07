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
package oscar.examples.visual

import oscar.visual.VisualFrame
import oscar.visual.VisualDrawing
import oscar.visual.shapes.VisualRectangle
import java.awt.Color
import oscar.visual.VisualBinPacking
import scala.util.Random
import oscar.visual.map.VisualMap
import oscar.visual.map.Geocoder
import oscar.visual.map.Location
import java.io.IOException
import oscar.visual.plot.PlotLine
import oscar.visual.map.Geocoder

object DemoVisu {

  val f = VisualFrame("Results");
  def runInThread(p: => Unit) = {
    val thread = new Thread(new Runnable {
      def run = p
    })
    thread.start
  }

  def main(args: Array[String]): Unit = {

    val tb = f.createToolBar()

    tb.addButton("rectangles", { runInThread(demoRectangles) })
    tb.addButton("plot", { runInThread(demoPlot) })
    tb.addButton("bin packing", { runInThread(demoBinPacking) })
    tb.addButton("map", { runInThread(demoMap) })
    f.pack()

  }

  def demoBinPacking = {

    val binPacking = VisualBinPacking()
    f.createFrame("Bin Packing").add(binPacking)
    f.pack()

    val rand = new scala.util.Random
    val items = Array.tabulate(20)(i => binPacking.addItem(i, rand.nextInt(91) + 10))

    for (_ <- 0 until 100) {
      val item = rand.nextInt(items.size)
      val bin = rand.nextInt(10)
      items(item).bin = bin
      Thread.sleep(500)
    }
  }

  def demoRectangles = {
    val d = VisualDrawing(false);
    val inf = f.createFrame("Rectangle");
    inf.add(d);
    f.pack();
    val rect = new VisualRectangle(d, 50, 50, 100, 50);

    Thread.sleep(1000);
    rect.innerCol_$eq(Color.red);
    Thread.sleep(1000);
    rect.width = 200;
    Thread.sleep(1000);
    rect.height = 100;
    Thread.sleep(1000);
    rect.translate(100, 20);
    Thread.sleep(1000);

    val test = new VisualRectangle(d,5,5,5,5)
    test.innerCol_$eq(Color.black)
    for (i <- 0 until 20) {
      Thread.sleep(50);
      rect.translate(rect.x + 5, rect.y);
    }
    rect.toolTip_$eq("Hello");
  }

  def demoPlot = {
    val inf = f.createFrame("Plot");
    val demo = new PlotLine("My Plot", "xlab", "ylab");
    inf.add(demo);
    inf.pack();

    for (i <- 0 to 10) {
      demo.addPoint(i, Math.random);
      Thread.sleep(1000);
    }

  }
  def demoMap = {
    val m = new VisualMap();
    val inf = f.createFrame("VisualMap");
    inf.add(m);
    f.pack();

    try {
      val citiesNames = List("Namur", "Bruxelles", "Antwerpen", "Arlon", "Mons", "Ottignies", "London")
      var citiesCoord = List[Location]()
      for (c <- citiesNames) {

        val loc = Geocoder.getLocation(c)
        citiesCoord = citiesCoord :+ loc
        m.createWaypoint(loc.lat, loc.lon);
        try {
          Thread.sleep(100)
        } catch {
          case e: InterruptedException => e.printStackTrace()
        }
      }

      val l = m.createLine((citiesCoord(6).lat, citiesCoord(6).lon), (citiesCoord(1).lat, citiesCoord(1).lon));

      citiesCoord.zipWithIndex.filter(p => p._2 != 1 && p._2 != 6).map(_._1).foreach(lo => m.createPath((lo.lat, lo.lon), (citiesCoord(1).lat, citiesCoord(1).lon)))

    } catch {
      case e1: IOException => e1.printStackTrace()
    }

    m.refresh()

  }
}

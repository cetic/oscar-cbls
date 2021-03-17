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
package oscar.visual

import java.awt.{BorderLayout, Color, Graphics, Graphics2D}
import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener}
import java.awt.geom.{AffineTransform, Point2D}

import javax.swing.{JPanel, SwingUtilities}
import oscar.visual.shapes.{VisualLine, VisualRectangle, VisualShape}

import scala.collection.mutable

/**
  * VisualDrawing
  *
  *  Contains and draws VisualShapes.
  */
class VisualDrawing(flipped: Boolean, scalable: Boolean) extends JPanel(new BorderLayout()) {

  setBackground(Color.white)

  // Shapes contained in the panel
  protected val shapes: mutable.Queue[VisualShape] = mutable.Queue()

  protected var marginT: Double = 0
  protected var marginR: Double = 0
  protected var marginB: Double = 0
  protected var marginL: Double = 0

  /** Returns the margins of the panel. */
  def margin: (Double, Double, Double, Double) = (marginT, marginR, marginB, marginL)

  /** Sets the margins of the panel. */
  def margin(m: Double): Unit = margin(m, m, m, m)

  /** Sets the margins of the panel. */
  def margin(top: Double, right: Double, bottom: Double, left: Double): Unit = {
    marginT = top
    marginR = right
    marginB = bottom
    marginL = left
  }

  // Returns the bounds of the bounding box containing all the shapes.
  protected def findBounds(shapes: Iterable[VisualShape]): (Double, Double, Double, Double) = {
    var minX = Double.MaxValue
    var maxX = Double.MinValue
    var minY = Double.MaxValue
    var maxY = Double.MinValue

    //RDL: added the try catch because there iss a race condition on shapes, which is a mutable data structure
    //and this paint method is called in a thread that is not hte one that adds the figures.
    //I do not know how to fix this issue,
    // but I am a bit bored by the big red exception messages I get on the console,
    //so I fix the symptom with this try catch
    try {
      for (shape <- shapes) {
        val bounds = shape.getBounds
        if (bounds._1 < minX) minX = bounds._1
        if (bounds._2 > maxX) maxX = bounds._2
        if (bounds._3 < minY) minY = bounds._3
        if (bounds._4 > maxY) maxY = bounds._4
      }
    }catch{
      case _:java.lang.IllegalArgumentException => ;
      case _:java.util.NoSuchElementException =>;
      case _:java.lang.NullPointerException => ;
    }

    (minX, maxX, minY, maxY)
  }
  var transform = new AffineTransform()
  var scale = 1.0

  override def paint(g: Graphics): Unit = {

    super.paintComponent(g)
    val g2d = g.asInstanceOf[Graphics2D]
    transform = new AffineTransform() // start with identity transform   

    if (shapes.nonEmpty) {

      // Shapes size
      val (minX, maxX, minY, maxY) = findBounds(shapes)
      val sWidth = maxX - minX
      val sHeight = maxY - minY

      // Drawing size
      val dWidth = getWidth
      val dHeight = getHeight

      // Flip
      if (flipped) {
        transform.translate(0, dHeight)
        transform.scale(1*scale, -1*scale)
      } else {
        transform.scale(1*scale, 1*scale)
      }

      // Scale
      if (scalable) {
        // Compute the scaling ratio
        val ratioX = dWidth / (marginR + marginL + sWidth)
        val ratioY = dHeight / (marginT + marginB + sHeight)
        val ratio = math.min(ratioX, ratioY) // Maintain proportions
        transform.scale(ratio, ratio)

        // Translate
        val translateX: Int = (marginL - minX).toInt
        val translateY: Int = ((if (flipped) marginB else marginT) - minY).toInt
        transform.translate(translateX,translateY)
      }
      g2d.transform(transform)

      //RDL: added the try catch because there iss a race condition on shapes, which is a mutable data structure
      //and this paint method is called in a thread that is not hte one that adds the figures.
      //I do not know how to fix this issue,
      // but I am a bit bored by the big red exception messages I get on the console,
      //so I fix the symptom with this try catch
      try {
        for (s <- shapes) {
          s.draw(g2d)
        }
      }catch{
        case _:java.lang.IllegalArgumentException => ;
        case _:java.util.NoSuchElementException =>
      }
    }
  }

  def invertTransform(p: Point2D): Point2D = {
    val clone = transform.clone().asInstanceOf[AffineTransform]
    clone.invert()
    clone.transform(new Point2D.Double(p.getX, p.getY), null)
  }

  /** Adds a new non null colored shape in the panel. */
  def addShape(shape: VisualShape, repaintAfter: Boolean = true): Unit = {
    if (shape == null) throw new IllegalArgumentException("The added shape is null.")
    else {
      shapes.enqueue(shape)
      if (repaintAfter) repaint()
    }
  }

  /** Removes all the shapes contained in the panel. */
  def clear(repaintAfter: Boolean = true): Unit = {
    shapes.clear()
    revalidate()
    if (repaintAfter) repaint()
  }

  addMouseMotionListener {
    val drawingPanel = this
    new MouseMotionListener() {
      override def mouseMoved(e: MouseEvent): Unit = {
        drawingPanel.setToolTipText("")
        for (s <- shapes) {
          s.showToolTip(e.getPoint)
        }
      }
      override def mouseDragged(e: MouseEvent): Unit = {}
    }
  }

  private def scale(factor: Double): Unit = {
    scale = scale * factor
    repaint()
  }

  addMouseListener {
    new MouseListener() {
      override def mouseClicked(e: MouseEvent): Unit = {
        if (SwingUtilities.isRightMouseButton(e)) {
          scale(0.9)
        }
        if (SwingUtilities.isLeftMouseButton(e)) {
          if (e.getClickCount == 2) {
            scale(1.1)
          }
          else {
            shapes.foreach(_.clicked(e.getPoint))
          }
        }
      }
      override def mouseEntered(e: MouseEvent): Unit = {}
      override def mousePressed(e: MouseEvent): Unit = {}
      override def mouseExited(e: MouseEvent): Unit = {}
      override def mouseReleased(e: MouseEvent): Unit = {}
    }
  }

  def showToolTip(text: String): Unit = {
    setToolTipText(text)
  }
}

object VisualDrawing {

  def apply(flipped: Boolean = true, scalable: Boolean = false): VisualDrawing = {
    new VisualDrawing(flipped, scalable)
  }
}

object VisualDrawingTest extends App {

  val frame = VisualFrame("Example")
  val drawing = VisualDrawing()
  val inFrame = frame.createFrame("Drawing")
  inFrame.add(drawing)
  frame.pack()

  val rect = new VisualRectangle(drawing, 50, 50, 100, 100)
  val line = VisualLine(drawing, 50, 50, 150, 150)

  try {
    Thread.sleep(1000)
  } catch {
    case e: InterruptedException => e.printStackTrace()
  }

  rect.innerCol = Color.red
}


package oscar.visual.shapes

import java.awt.geom.Ellipse2D
import java.awt.Color
import oscar.visual.VisualDrawing
import oscar.visual.VisualFrame

class VisualCircle(d: VisualDrawing, x: Double, y: Double, r: Double) extends VisualShape(d) {

  type S = Ellipse2D.Double
  protected val shape = new Ellipse2D.Double(x-r,y-r,2*r,2*r)
  
  private var radius: Double = r
  private var centerX: Double = x
  private var centerY: Double = y

  override def moveAt(x: Double, y: Double): Unit = {
    centerX = x
    centerY = y
    update()
  }

  def translate(x: Double, y: Double): Unit = {
    centerX += x
    centerY += y
    update()
  }
  
  def getX: Double = centerX
  def getY: Double = centerY
  
  def getRadius: Double = radius
  
  def setRadius(radius: Double): Unit = {
    this.radius = radius
    update()
  }
  
  private def update(): Unit = {
    shape.setFrame(centerX-radius, centerY-radius, 2*radius, 2*radius)
    drawing.repaint()
  }
}

object VisualCircle {
  
  def apply(drawing: VisualDrawing, centerX: Double, centerY: Double, radius: Double): VisualCircle = {
    new VisualCircle(drawing, centerX, centerY, radius)
  }
}

object VisualCircleExample extends App {
  
  val frame = VisualFrame("Example")
  val drawing = VisualDrawing(false)
  val inFrame = frame.createFrame("Drawing")
  inFrame.add(drawing)
  frame.pack()
  
  val circ = VisualCircle(drawing, 100, 100, 50)
  Thread.sleep(1000)
  circ.innerCol = Color.blue
  Thread.sleep(1000)
  circ.setRadius(100)
  Thread.sleep(1000)
  circ.translate(300,200)
  for (i <- 1 to 20) {
    Thread.sleep(50)
    circ.translate(circ.getX+5, circ.getY)
  }
}

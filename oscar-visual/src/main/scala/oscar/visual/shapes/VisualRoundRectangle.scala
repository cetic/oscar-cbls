package oscar.visual.shapes

import java.awt.geom.RoundRectangle2D
import oscar.visual.VisualDrawing

class VisualRoundRectangle(d: VisualDrawing, s: RoundRectangle2D.Double) extends VisualShape(d) {

  type S = RoundRectangle2D.Double
  protected val shape: RoundRectangle2D.Double = s
  
  def x: Double = shape.getX
  def y: Double = shape.getY
  
  def width: Double = shape.getWidth
  def height: Double = shape.getHeight
  
  def arcWidth: Double = shape.getArcWidth
  def arcHeight: Double = shape.getArcHeight
  
  def arcWidth_= (w: Double): Unit = { 
    shape.setRoundRect(x, y, width, height, w, arcHeight)
    drawing.repaint()
  }
  
  def arcHeight_= (h: Double): Unit = {
    shape.setRoundRect(x, y, width, height, arcWidth, h)
    drawing.repaint()
  }
  
  def move(x: Double, y: Double): Unit = {
    shape.setRoundRect(x, y, width, height, arcWidth, arcHeight)
  }
}

object VisualRoundRectangle {  
  def apply(drawing: VisualDrawing, x: Double, y: Double, width: Double, height: Double, arcWidth: Double = 10, arcHeight: Double = 10): VisualRoundRectangle = {
    val roundRec = new RoundRectangle2D.Double(x, y, width, height, arcWidth, arcHeight)
    new VisualRoundRectangle(drawing, roundRec)
  }
}

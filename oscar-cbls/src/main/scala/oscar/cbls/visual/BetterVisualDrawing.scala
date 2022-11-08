package oscar.cbls.visual

import oscar.visual.VisualDrawing
import oscar.visual.shapes.VisualText

import java.awt.Point
import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener, MouseWheelEvent, MouseWheelListener}

class BetterVisualDrawing(flipped: Boolean, scalable: Boolean) extends VisualDrawing(flipped,scalable) {

  private def horizontalOverflow: Double = {
    if(shapes.nonEmpty) {
      val shapeMaxWidth = (shapes.map(_.getBounds._2).max - shapes.map(_.getBounds._1).min) * scale
      shapeMaxWidth - getWidth
    } else 0
  }

  private def verticalOverflow: Double = {
    if (shapes.nonEmpty) {
      val shapeMaxHeight = (shapes.map(_.getBounds._4).max - shapes.map(_.getBounds._3).min) * scale
      shapeMaxHeight - getHeight
    } else 0
  }

  def getMaximumH: Int = {
    if (shapes != null && shapes.nonEmpty) Math.max(1,((horizontalOverflow / getWidth).ceil * 10).toInt)
    else 1
  }

  def getMaximumV: Int = {
    if (shapes != null && shapes.nonEmpty) Math.max(1,((verticalOverflow / getHeight).ceil * 10).toInt)
    else 1
  }

  // LISTENERS
  private var mousePressedAt: Point = _
  this.removeMouseListener(this.getMouseListeners.head)
  this.addMouseListener(new MouseListener {
    override def mouseClicked(mouseEvent: MouseEvent): Unit = {}

    override def mousePressed(mouseEvent: MouseEvent): Unit = mousePressedAt = mouseEvent.getPoint

    override def mouseReleased(mouseEvent: MouseEvent): Unit = {}

    override def mouseEntered(mouseEvent: MouseEvent): Unit = {}

    override def mouseExited(mouseEvent: MouseEvent): Unit = {}
  })
  this.addMouseWheelListener(new MouseWheelListener {
    override def mouseWheelMoved(mouseWheelEvent: MouseWheelEvent): Unit = {
      if(mouseWheelEvent.getWheelRotation == 1) {
        if (horizontalOverflow > 0 || verticalOverflow > 0)
          scale = scale - 0.1
      } else if(mouseWheelEvent.getWheelRotation == -1) {
        scale = scale + 0.1
      } else {
        println(s"WARNING : Unexpected wheel rotation value ${mouseWheelEvent.getWheelRotation}")
      }
      repaint()
    }
  })
  this.addMouseMotionListener {
    new MouseMotionListener() {
      override def mouseMoved(e: MouseEvent): Unit = {}
      override def mouseDragged(e: MouseEvent): Unit = {
        val diffX = e.getX - mousePressedAt.x
        val diffY = e.getY - mousePressedAt.y
        for (shape <- shapes)
          shape.moveAt(shape.getBounds._1+diffX, shape.getBounds._3+diffY)
        mousePressedAt = e.getPoint
      }
    }
  }

  def resize(): Unit ={
    this.repaint()
    this.revalidate()
  }

}

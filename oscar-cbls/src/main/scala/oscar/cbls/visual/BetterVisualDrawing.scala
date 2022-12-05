package oscar.cbls.visual

import oscar.visual.VisualDrawing
import oscar.visual.shapes.VisualText

import java.awt.{Dimension, Point}
import java.awt.event.{MouseAdapter, MouseEvent, MouseListener, MouseMotionListener, MouseWheelEvent, MouseWheelListener}
import javax.swing.{JScrollPane, JViewport, SwingUtilities}

class BetterVisualDrawing(flipped: Boolean, scalable: Boolean) extends VisualDrawing(flipped,scalable) {

  // LISTENERS
  private var mousePressedAt: Option[Point] = None
  this.removeMouseListener(this.getMouseListeners.head)
  private val _this = this

  private val mouseAdapter: MouseAdapter = new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = {
      mousePressedAt = Some(new Point(e.getPoint))
    }
    override def mouseDragged(e: MouseEvent): Unit = {
      if(mousePressedAt.isDefined){
        val viewPort = SwingUtilities.getAncestorOfClass(classOf[JViewport], _this).asInstanceOf[JViewport]
        if (viewPort != null) {
          val deltaX = mousePressedAt.get.x - e.getX
          val deltaY = mousePressedAt.get.y - e.getY
          val view = viewPort.getViewRect
          view.x += deltaX
          view.y += deltaY
          _this.scrollRectToVisible(view)
        }
      }
    }
  }
  this.addMouseListener(mouseAdapter)
  this.addMouseMotionListener(mouseAdapter)

  this.addMouseWheelListener(new MouseWheelListener {
    override def mouseWheelMoved(mouseWheelEvent: MouseWheelEvent): Unit = {
      if(mouseWheelEvent.getWheelRotation == 1) {
        if(scale > 0.5)
          scale = scale - 0.1
      } else if(mouseWheelEvent.getWheelRotation == -1) {
        scale = scale + 0.1
      } else {
        println(s"WARNING : Unexpected wheel rotation value ${mouseWheelEvent.getWheelRotation}")
      }
      resize()
    }
  })

  override def getPreferredSize: Dimension = {
    val bounds = this.findBounds(shapes)
    new Dimension(((bounds._2-bounds._1)*scale).toInt, ((bounds._4-bounds._3)*scale).toInt)
  }

  def resize(): Unit ={
    this.repaint()
    this.revalidate()
  }

}
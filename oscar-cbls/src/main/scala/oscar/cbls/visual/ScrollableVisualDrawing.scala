package oscar.cbls.visual

import oscar.visual.VisualDrawing

import java.awt.{BorderLayout, Scrollbar}
import java.awt.event.{AdjustmentEvent, AdjustmentListener}

class ScrollableVisualDrawing(flipped: Boolean, scalable: Boolean) extends VisualDrawing(flipped,scalable) {

  private def horizontalOverflow: Double = {
    if(shapes.nonEmpty) {
      val shapeMaxWidth = (shapes.map(_.getBounds._2).max - shapes.map(_.getBounds._1).min) * scale
      shapeMaxWidth - getWidth + scrollBVertical.getWidth + 20
    } else 0
  }

  private def verticalOverflow: Double = {
    if (shapes.nonEmpty) {
      val shapeMaxHeight = (shapes.map(_.getBounds._4).max - shapes.map(_.getBounds._3).min) * scale
      shapeMaxHeight - getHeight + scrollBHorizontal.getHeight + 20
    } else 0
  }

  private def pixelPerScrollValue(direction: String): Double ={
    direction match {
      case "V" => verticalOverflow/scrollBVertical.getMaximum
      case "H" => horizontalOverflow/scrollBHorizontal.getMaximum
    }
  }

  def getMaximumH: Int = {
    if (shapes != null && shapes.nonEmpty) Math.max(1,((horizontalOverflow / getWidth).ceil * 10).toInt)
    else 1
  }

  def getMaximumV: Int = {
    if (shapes != null && shapes.nonEmpty) Math.max(1,((verticalOverflow / getHeight).ceil * 10).toInt)
    else 1
  }

  val scrollBHorizontal = new Scrollbar(Scrollbar.HORIZONTAL, 1, 100, 0, getMaximumH)
  val scrollBVertical = new Scrollbar(Scrollbar.VERTICAL, 1, 100, 0, getMaximumV)

  scrollBHorizontal.addAdjustmentListener(new AdjustmentListener {
    private var oldValue = 0
    private var oldPixelPerScrollBValue = pixelPerScrollValue("H")
    override def adjustmentValueChanged(adjustmentEvent: AdjustmentEvent): Unit = {
      val newValue = adjustmentEvent.getValue
      val newPixelPerScrollBValue = pixelPerScrollValue("H")
      for (shape <- shapes)
        shape.moveAt(shape.getBounds._1+((oldPixelPerScrollBValue*oldValue)-(newPixelPerScrollBValue*newValue)).round, shape.getBounds._3)
      oldValue = newValue
      oldPixelPerScrollBValue = newPixelPerScrollBValue
    }
  })

  scrollBVertical.addAdjustmentListener(new AdjustmentListener {
    private var oldValue = 0
    private var oldPixelPerScrollBValue = pixelPerScrollValue("V")
    override def adjustmentValueChanged(adjustmentEvent: AdjustmentEvent): Unit = {
      val newValue = adjustmentEvent.getValue
      val newPixelPerScrollBValue = pixelPerScrollValue("V")
      for (shape <- shapes)
        shape.moveAt(shape.getBounds._1, shape.getBounds._3+((oldPixelPerScrollBValue*oldValue)-(newPixelPerScrollBValue*newValue)).round)
      oldValue = newValue
      oldPixelPerScrollBValue = newPixelPerScrollBValue
    }
  })

  this.add(scrollBHorizontal, BorderLayout.SOUTH)
  this.add(scrollBVertical, BorderLayout.EAST)

  def resize(): Unit ={
    val newMaxH = getMaximumH
    val newMaxV = getMaximumV
    scrollBHorizontal.setVisible(newMaxH > 1)
    scrollBHorizontal.setMaximum(newMaxH)
    scrollBHorizontal.setValue(0)
    scrollBVertical.setVisible(newMaxV > 1)
    scrollBVertical.setMaximum(newMaxV)
    scrollBVertical.setValue(0)
    this.repaint()
    this.revalidate()
  }

}

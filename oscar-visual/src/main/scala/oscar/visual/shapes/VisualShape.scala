package oscar.visual.shapes

import java.awt.Color
import java.awt.Graphics2D
import java.awt.geom.Point2D
import oscar.visual.VisualDrawing
import java.awt.BasicStroke

abstract class VisualShape(protected val drawing: VisualDrawing) {

  type S <: java.awt.Shape
  protected val shape: S
  
  private var onClickActions = List[() => Unit]()
  
  def onClick(action: => Unit): Unit = {
    onClickActions = (() => action) :: onClickActions
  }
  
  // Adds the visual shape in the drawing
  drawing.addShape(this)

  // True if the border is dashed
  private var _dashed: Boolean = false
  // Width of the border
  private var _bWidth: Float = 1

  private var _fillColor: Color = Color.white
  private var _borderColor: Color = Color.black

  private var _visible: Boolean = true
  private var _fill: Boolean = true
  private var _border: Boolean = true

  private var _toolTipText: Option[String] = None

  // If true, the drawing repaints after each modification of the shape
  private var _autoRepaint: Boolean = true

  def autoRepaint: Boolean = _autoRepaint
  
  def autoRepaint_=(b: Boolean): Unit = { _autoRepaint = b }

  def dashed: Boolean = _dashed
  
  def dashed_=(dashed: Boolean): Unit = {
    _dashed = dashed
    if (autoRepaint) repaint()
  }

  def borderWidth: Double = _bWidth
  
  def borderWidth_=(width: Float): Unit = {
    _bWidth = width
    if (autoRepaint) repaint()
  }

  def innerCol: Color = _fillColor

  def innerCol_=(innerCol: Color): Unit = {
    _fillColor = innerCol
    if (autoRepaint) repaint()
  }

  def outerCol: Color = _borderColor

  def outerCol_=(outerCol: Color): Unit = {
    _borderColor = outerCol
    if (autoRepaint) repaint()
  }

  def visible: Boolean = _visible

  def visible_=(visible: Boolean): Unit = {
    _visible = visible
    if (autoRepaint) repaint()
  }

  def border: Boolean = _border

  def border_=(border: Boolean): Unit = {
    _border = border
    if (autoRepaint) repaint()
  }

  def fill: Boolean = _fill

  def fill_=(fill: Boolean): Unit = {
    _fill = fill
    if (autoRepaint) repaint()
  }

  def toolTip: String = _toolTipText match {
    case Some(s) => s
    case None => ""
  }

  def toolTip_=(s: String): Unit = { _toolTipText = Some(s) }

  def showToolTip(mousePoint: Point2D): Unit = {
    if (_toolTipText.isDefined && shape.contains(drawing.invertTransform(mousePoint))) {
      drawing.showToolTip(toolTip)
    }
  }
  
  def clicked(mousePoint: Point2D): Unit = {
    if (shape.contains(drawing.invertTransform(mousePoint))) {
      onClickActions.foreach(action => action())
    }
  }  

  protected def dashedStroke: BasicStroke = {
    new BasicStroke(_bWidth, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER, 10.0f, Array(_bWidth / 2, 2 * _bWidth), 0)
  }

  protected def plainStroke: BasicStroke = {
    new BasicStroke(_bWidth)
  }

  protected def stroke: BasicStroke = {
    if (_dashed) dashedStroke else plainStroke
  }

  def repaint(): Unit = drawing.repaint()

  
  def draw(g: Graphics2D): Unit = {
    if (visible) {
      if (fill) {
        g.setColor(_fillColor)
        g.fill(shape)
      }
      if (border) {
        val oldStroke = g.getStroke
        g.setStroke(stroke)
        g.setColor(_borderColor)
        g.draw(shape)
        g.setStroke(oldStroke)
      }
    }
  }

  def getBounds: (Int, Int, Int, Int) = {
    val bounds = shape.getBounds
    val xMin = bounds.x
    val xMax = xMin + bounds.width
    val yMin = bounds.y
    val yMax = yMin + bounds.height
    (xMin, xMax, yMin, yMax)
  }

  def move(x: Double, y: Double): Unit
}




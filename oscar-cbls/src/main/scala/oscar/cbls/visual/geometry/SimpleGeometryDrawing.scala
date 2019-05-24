
package oscar.cbls.visual.geometry

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

import java.awt.event.{ComponentEvent, ComponentListener, KeyEvent, KeyListener}
import java.awt.{Color, Shape}
import java.awt.image.BufferedImage
import java.io.File
import java.time.LocalDateTime

import javax.imageio.ImageIO
import org.locationtech.jts.awt.ShapeWriter
import org.locationtech.jts.geom._
import org.locationtech.jts.geom.util.AffineTransformation
import oscar.cbls.business.geometry
import oscar.cbls.business.geometry.invariants.Overlap
import oscar.visual.VisualDrawing
import oscar.visual.shapes.VisualShape


class SimpleGeometryDrawing(relevantDistances:List[(Int,Int)],
                            windowWidth: Int = 960,
                            windowHeight: Int = 960,
                            pointShift: Option[() => (Double,Double)] = None,
                            savingFile: Option[File] = None) //TODO: pointShift:(Double,Double) = (0,0) ce serait nettement plus simple.
  extends VisualDrawing(false,false)
    with GeometryDrawingTrait {

  this.setSize(windowWidth, windowHeight)
  this.setFocusable(true)

  var geometryShapes: List[(Geometry,Option[Color],Option[Color],String)] = List.empty
  var centers: List[(Long, Long)] =  List.empty
  var boundingBoxOn: Option[Geometry] = None

  class VisualShapeConcrete(d: VisualDrawing, s: Shape) extends VisualShape(d) {

    override def move(x: Double, y: Double): Unit = {}

    type S = Shape
    protected val shape = s
  }


  override def addShape(shape: VisualShape, repaintAfter: Boolean): Unit ={
    super.addShape(shape,false)
  }

  //TODO: il manque un zoom et un scroll!!!
  //TODO: on ne voit pas le border!!
  /**
    * @param shapes shape,bordercolor,innercolor,toltipText
    */
  def drawShapes(boundingBoxOn:Option[Geometry] = None,
                 shapes:List[(Geometry,Option[Color],Option[Color],String)],
                 centers:List[(Long,Long)],
                 saveShapesAndPositions: Boolean = true) ={
    super.clear(false)

    val w = new ShapeWriter()

    val (minX,maxX,minY,maxY) = boundingBoxOn match {
      case None =>
        (shapes.map(_._1.getCoordinates.map(c => c.x).min).min,
          shapes.map(_._1.getCoordinates.map(c => c.x).max).max,
          shapes.map(_._1.getCoordinates.map(c => c.y).min).min,
          shapes.map(_._1.getCoordinates.map(c => c.y).max).max)
      case Some(b) =>
        (b.getCoordinates.map(c => c.x).min,
          b.getCoordinates.map(c => c.x).max,
          b.getCoordinates.map(c => c.y).min,
          b.getCoordinates.map(c => c.y).max)
    }

    val (scaleTransform, translateTransform) = defineDrawingAreaAndAffineTransformation(minX, maxX, minY, maxY)

    val boundingBoxOnNow = boundingBoxOn.getOrElse(
      new Polygon(
        geometry.factory.createLinearRing(List(new Coordinate(minX,0), new Coordinate(0,minY), new Coordinate(maxX,0), new Coordinate(0,maxY)).toArray),
        null,
        geometry.factory))
    paintBorder(w, boundingBoxOnNow, scaleTransform, translateTransform)

    paintShapes(w, shapes, scaleTransform, translateTransform)
    paintLinks(w, centers, scaleTransform, translateTransform)

    if(saveShapesAndPositions){
      geometryShapes = shapes
      this.centers = centers
      this.boundingBoxOn = boundingBoxOn
    }
    //repaint()
  }

  private def defineDrawingAreaAndAffineTransformation(minX: Double, maxX: Double, minY: Double, maxY: Double): (AffineTransformation,AffineTransformation) ={
    val drawingWidth = (maxX - minX).toInt
    val drawingHeight = (maxY - minY).toInt


    val (realMapScaleX,realMapScaleY) = pointShift.getOrElse(() => (0.0,0.0))()

    val widthOffSet = if(realMapScaleX > 0) realMapScaleX else 10.0
    val heightOffSet = if(realMapScaleY > 0) realMapScaleY else 10.0

    val scaling = Math.min((this.getWidth.toDouble-(widthOffSet*2.0))/drawingWidth,(this.getHeight.toDouble-(2.0*heightOffSet))/drawingHeight)
    val scaleTransform = AffineTransformation.scaleInstance(scaling,scaling)
    val translateTransform = AffineTransformation.translationInstance(widthOffSet,heightOffSet)

    (scaleTransform, translateTransform)
  }

  // Paint the box containing all the shapes
  private def paintBorder(w: ShapeWriter, boundingBoxOn: Geometry, scaleTransform: AffineTransformation, translateTransform:AffineTransformation): Unit ={
    val border = new VisualShapeConcrete(this, w.toShape(translateTransform.transform(scaleTransform.transform(boundingBoxOn))))

    border.fill = false
    border.border = true
    border.borderWidth = 3
    border.outerCol = Color.black
  }

  /**
    * Draw the shapes of the problem
    * @param w The shapeWriter
    * @param shapes The shapes
    * @param scaleTransform The scaling affine transformation
    * @param translateTransform The translating affine transformation
    */
  private def paintShapes(w: ShapeWriter, shapes:List[(Geometry,Option[Color],Option[Color],String)], scaleTransform: AffineTransformation, translateTransform:AffineTransformation): Unit ={
    for((geometry,borderColOpt,innerColOpt,toolTipText) <- shapes){
      val s = new VisualShapeConcrete(this, w.toShape(translateTransform.transform(scaleTransform.transform(geometry))))

      borderColOpt match{
        case None =>
          s.border = false
        case Some(c) =>
          s.border = true
          s.outerCol = c
      }
      innerColOpt match{
        case None =>
          s.fill = false
        case Some(c) =>
          s.fill = true
          s.innerCol = c
      }

      s.toolTip = toolTipText
    }
  }

  private def paintSomeHoles(s:List[Geometry],scaleTransform:AffineTransformation, translateTransform:AffineTransformation): Unit ={
    val w = new ShapeWriter()

    val centers = Overlap.centersOfFreeSpaces(s.tail, s.head,100).toArray

    for(center <- centers){
      val centroidPoint = new VisualShapeConcrete(this, w.toShape(translateTransform.transform(scaleTransform.transform(geometry.point(center._1,center._2)))))
      centroidPoint.fill = true
      centroidPoint.innerCol = Color.BLACK
      centroidPoint.borderWidth = 10
      centroidPoint.border = true
      centroidPoint.outerCol = Color.BLACK

    }
  }

  // Paint links between shapes
  private def paintLinks(w: ShapeWriter, centers: List[(Long,Long)], scaleTransform: AffineTransformation, translateTransform:AffineTransformation): Unit ={
    for((fromID,toID) <- relevantDistances){
      val (x1,y1) = centers(fromID)
      val (x2,y2) = centers(toID)
      val s = new VisualShapeConcrete(this, w.toShape(translateTransform.transform(scaleTransform.transform(geometry.createLine(x1:Long,y1:Long,x2:Long,y2:Long)))))
      s.innerCol = Color.BLUE
      s.dashed = false
    }
  }

  private def coordToPixel(coord: (Long,Long), drawingWidth: Int, drawingHeight: Int): (Int,Int) ={
    val (shiftX,shiftY): (Double, Double)= if(pointShift.isDefined)pointShift.get.apply() else (0,0)
    val scaling = Math.max((this.getWidth.toDouble-(shiftX*2))/drawingWidth,(this.getHeight.toDouble-(shiftY*2))/drawingHeight)
    val x = ((coord._1.toDouble*scaling) + shiftX).toInt
    val y = ((coord._2.toDouble*scaling) + shiftY).toInt
    (x,y)
  }


  def saveStateAsPNG(): Unit ={
    val bi = new BufferedImage(this.getWidth, this.getHeight, BufferedImage.TYPE_INT_ARGB)
    val g = bi.createGraphics
    this.paint(g) //this == JComponent

    val now = LocalDateTime.now().toString
    val filePath = savingFile.get.getPath
    val initFileName = if(filePath.endsWith(".png")) filePath.dropRight(4) else filePath
    val fileName = initFileName + "_" + now + ".png"

    g.dispose()
    try
      ImageIO.write(bi,"png", new File(fileName))
    catch {
      case e: Exception =>

    }
  }

  addKeyListener{
    new KeyListener {
      override def keyTyped(keyEvent: KeyEvent): Unit = {
        if(keyEvent.getKeyChar.equals('p') && savingFile.isDefined){
          saveStateAsPNG()
        }
      }

      override def keyPressed(keyEvent: KeyEvent): Unit = {}

      override def keyReleased(keyEvent: KeyEvent): Unit = {}
    }
  }

  addComponentListener{
    new ComponentListener {
      override def componentResized(componentEvent: ComponentEvent): Unit = {
        if(shapes.nonEmpty)
          drawShapes(boundingBoxOn, geometryShapes, centers, false)
      }

      override def componentMoved(componentEvent: ComponentEvent): Unit = repaint()

      override def componentShown(componentEvent: ComponentEvent): Unit = repaint()

      override def componentHidden(componentEvent: ComponentEvent): Unit = repaint()
    }
  }
}



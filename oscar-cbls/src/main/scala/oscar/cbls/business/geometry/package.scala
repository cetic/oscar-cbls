package oscar.cbls.business


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


import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.util.AffineTransformation
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, MultiPolygon, Point}

package object geometry {

  // This is the general factory to use, it is kept in floats because putting it to integers
  // only adds an extra rounding at the end of each operation.
  // so better to keep it all in floats, and perform any conversion when getting things out of JTS
  val factory:GeometryFactory = new GeometryFactory()

  val emptyPolygon:MultiPolygon = factory.createMultiPolygon(Array.empty)

  def point(x:Double,y:Double):Point = {
    new Point(new CoordinateArraySequence(Array(new Coordinate(x,y))), factory)
  }

  val emptyGeometryValue = new GeometryValue(
    emptyPolygon,
    inputCentreOfOverApproximatingCircle = None,
    inputOverApproximatingRadius = Some(0))

  def createLine(x1:Double,y1:Double,x2:Double,y2:Double):Geometry = {
    geometry.factory.createLineString(Array(
      new Coordinate(x1,y1),
      new Coordinate(x2,y2)
    ))
  }
  def createLineFromCoordinates(a:Coordinate,b:Coordinate):Geometry = {
    geometry.factory.createLineString(Array(a,b))
  }

  def createSquare(side:Double):GeometryValue = {
    val halfSide = side/2

    val sq = geometry.factory.createLinearRing(Array(
      new Coordinate(halfSide,halfSide),
      new Coordinate(halfSide,-halfSide),
      new Coordinate(-halfSide,-halfSide),
      new Coordinate(-halfSide,halfSide),
      new Coordinate(halfSide,halfSide))).convexHull()

    new GeometryValue(
      sq,
      inputCentreOfOverApproximatingCircle = Some(point(0,0)),
      inputOverApproximatingRadius = Some(halfSide * math.sqrt(2.0)))
  }

  def createRectangle(height:Double,width:Double):GeometryValue = {

    val halfHeight = height /2
    val halfWidth = width/2

    val rec = geometry.factory.createLinearRing(Array(
      new Coordinate(halfWidth,halfHeight),
      new Coordinate(halfWidth,-halfHeight),
      new Coordinate(-halfWidth,-halfHeight),
      new Coordinate(-halfWidth,halfHeight),
      new Coordinate(halfWidth,halfHeight))).convexHull()

    new GeometryValue(
      rec,
      inputCentreOfOverApproximatingCircle = Some(point(0,0)),
      inputOverApproximatingRadius = Some(math.sqrt((halfHeight * halfHeight) + (halfWidth * halfWidth))))
  }

  def createCircle(r:Double,nbEdges:Int = 16, ensureCorrectSurface:Boolean = true):GeometryValue = {
    require(nbEdges >=4,"a circle is hard to approximate with less than four edges...")

    val pointRotation = AffineTransformation.rotationInstance((2*math.Pi)/nbEdges)

    val startPoint = new Coordinate(r, 0)
    var currentPoint:Coordinate = startPoint
    val allPoints:Array[Coordinate] = Array.fill(nbEdges+1)(null)
    allPoints(0) = currentPoint
    allPoints(nbEdges) = currentPoint

    for(currentIndice <- 1 until nbEdges){
      currentPoint = pointRotation.transform(currentPoint,new Coordinate(0, 0))
      allPoints(currentIndice) = currentPoint
    }

    val c1 = factory.createPolygon(allPoints)

    if(ensureCorrectSurface){
      val s1 = c1.getArea  //TODO: this is crappy slow; hopefully called only once ?
      val factor = math.sqrt(math.Pi*r*r/s1)
      val scaling = AffineTransformation.scaleInstance(factor,factor)

      new GeometryValue(
        scaling.transform(c1),
        inputCentreOfOverApproximatingCircle = Some(point(0,0)),
        inputOverApproximatingRadius = Some(factor * r))

    }else{
      new GeometryValue(
        c1,
        inputCentreOfOverApproximatingCircle = Some(point(0,0)),
        inputOverApproximatingRadius = Some(r))
    }
  }

  type CBLSGeometryConst = oscar.cbls.business.geometry.model.CBLSGeometryConst
  final val CBLSGeometryConst = oscar.cbls.business.geometry.model.CBLSGeometryConst

  type CBLSGeometryVar = oscar.cbls.business.geometry.model.CBLSGeometryVar
  final val CBLSGeometryVar = oscar.cbls.business.geometry.model.CBLSGeometryVar

  type GeometryValue = oscar.cbls.business.geometry.model.GeometryValue
  final val GeometryValue = oscar.cbls.business.geometry.model.GeometryValue

}


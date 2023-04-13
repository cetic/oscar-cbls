package oscar.cbls.business.geometry.algo

import org.locationtech.jts.algorithm.distance.PointPairDistance
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryCollection, LineSegment, LineString, Polygon}

object MaxDistance {

  def computeDistance(geom: Geometry,
                      pt: Coordinate,
                      ptDist:PointPairDistance = new PointPairDistance()): PointPairDistance = {
    geom match {
      case l:LineString =>
        computeDistanceLineString(l, pt, ptDist)
      case p:Polygon =>
        computeDistancePolygon(p, pt, ptDist)
      case gc:GeometryCollection =>
        for(i <- 0 until gc.getNumGeometries){
          val g = gc.getGeometryN(i)
          computeDistance(g, pt, ptDist)
        }
      case _ =>
        ptDist.setMaximum(geom.getCoordinate, pt)
    }
    ptDist
  }

  def computeDistanceLineString(line: LineString, pt: Coordinate, ptDist: PointPairDistance): Unit = {
    //val tempSegment = new LineSegment
    val coords:Array[Coordinate] = line.getCoordinates

    for(coord <- coords){
      ptDist.setMaximum(coord, pt)
    }
  }

  def computeDistanceLineSegment(segment: LineSegment, pt: Coordinate, ptDist: PointPairDistance): Unit = {
    ptDist.setMaximum(segment.p0, pt)
    ptDist.setMaximum(segment.p1, pt)
  }

  def computeDistancePolygon(poly: Polygon, pt: Coordinate, ptDist: PointPairDistance): Unit = {
    computeDistanceLineString(poly.getExteriorRing, pt, ptDist)
  }
}

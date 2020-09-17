package oscar.cbls.algo.graph

abstract sealed class ClosestCentroidLabeling{
  def <(that:ClosestCentroidLabeling):Boolean
  def equals(that:ClosestCentroidLabeling):Boolean
  def min(that:ClosestCentroidLabeling):ClosestCentroidLabeling = {
    if(this < that) this else that
  }
  def isUnreachabe:Boolean
}

case class VoronoiZone(centroid:Node, distance:Long, incomingEdge:Edge) extends ClosestCentroidLabeling{
  override def <(that: ClosestCentroidLabeling): Boolean = that match{
    case Unreachable => true
    case that:VoronoiZone =>
      this.distance < that.distance || (that.distance == this.distance && this.centroid.id < that.centroid.id)
  }

  override def equals(that: ClosestCentroidLabeling): Boolean = that match {
    case Unreachable => false
    case that:VoronoiZone => that.distance == this.distance && this.centroid == that.centroid
  }

  def + (edge:Edge):VoronoiZone = VoronoiZone(centroid,distance+edge.length,edge)

  override def toString: String = s"VoronoiZone(centroid:${centroid.id} distance:$distance incomingEdge:$incomingEdge)"

  override def isUnreachabe: Boolean = false
}

case object Unreachable extends ClosestCentroidLabeling{
  override def <(that: ClosestCentroidLabeling): Boolean = false

  override def equals(that: ClosestCentroidLabeling): Boolean = that match{
    case Unreachable => true
    case that:VoronoiZone => false
  }

  override def isUnreachabe: Boolean = true
}

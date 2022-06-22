package oscar.cbls.business.routing.invariants.naive

import oscar.cbls.core.computation.ChangingSeqValue
import oscar.cbls.algo.seq.IntSequenceExplorer
import oscar.cbls.business.routing.invariants.segments.{Segment,NewNode,PreComputedSubSequence,FlippedPreComputedSubSequence}
import oscar.cbls.business.routing.invariants.segments.VehicleSegments
import oscar.cbls.algo.quick.QList

abstract class BackwardNaiveRoutingConstraint[U <: Any : Manifest](routes : ChangingSeqValue,
                                                                   n : Int,
                                                                   v : Int,
                                                                   defaultValue : U ,
                                                                   initValuePerVehicle : Array[U],
                                                                   fonc : (Int,Int,U) => U)
  extends AbstractNaiveRoutingConstraint[U](routes,n,v,defaultValue,initValuePerVehicle,fonc) {

  override def getNextPointOfSegmentToUpdate(expl : IntSequenceExplorer,seg : Segment) : Option[IntSequenceExplorer] =  {
    seg match {
      case NewNode(_) => None
      case PreComputedSubSequence(fstNode,_,_) =>
        if (expl.value == fstNode) None else expl.prev
      case FlippedPreComputedSubSequence(fstNode,_,_) =>
        if (expl.value == fstNode) None else expl.prev
    }
  }

  override def firstPointToUpdateOnSegment(seg : Segment) : Int = {
    seg match {
      case NewNode(n) => n
      case PreComputedSubSequence(_,lstNode,_) => lstNode
      case FlippedPreComputedSubSequence(_,lstNode,_) => lstNode
    }
  }

  override def lastPointToUpdateOnSegment(seg : Segment) : Int = {
    seg match {
      case NewNode(n) => n
      case PreComputedSubSequence(fstNode,_,_) => fstNode
      case FlippedPreComputedSubSequence(fstNode,_,_) => fstNode
    }
  }

  override def ordonateSegments(segments : VehicleSegments) : QList[Segment] = {
    if (segments.segments != null)
      segments.segments.reverse
    else
      segments.segments
  }
}

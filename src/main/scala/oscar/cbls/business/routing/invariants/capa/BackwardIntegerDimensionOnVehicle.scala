package oscar.cbls.business.routing.invariants.capa

import oscar.cbls.business.routing.invariants.naive.BackwardNaiveRoutingConstraint
import oscar.cbls.core.computation.{CBLSIntVar, ChangingSeqValue, FullRange, Store}

/**
 * this is DRAFT
 */
object BackwardIntegerDimensionOnVehicle {
  def apply(routes: ChangingSeqValue,
            v: Int,
            m: Store,
            defaultForUnrouted:Int,
            finalValueAtReturn:Array[Int],
            fonc: (Int, Int, Int) => Int): BackwardIntegerDimensionOnVehicle = {
    val nbNodes = routes.maxValue + 1
    val content = Array.tabulate(nbNodes)(node => CBLSIntVar(m, 0, FullRange, s"content at node $node"))
    new BackwardIntegerDimensionOnVehicle(
      routes,
      nbNodes,
      v,
      defaultForUnrouted,
      finalValueAtReturn,
      fonc,
      content)
  }
}

class BackwardIntegerDimensionOnVehicle(routes : ChangingSeqValue,
                                      n : Int,
                                      v : Int,
                                      default4Unrouted : Int,
                                      initValue : Array[Int],
                                      fonc : (Int,Int,Int) => Int,
                                      val contentAtEachNode : Array[CBLSIntVar])
  extends BackwardNaiveRoutingConstraint[Int](routes,n,v,default4Unrouted,initValue,fonc) {

  contentAtEachNode.foreach(v => v.setDefiningInvariant(this))

  override def assignNodeValue(node : Int,value : Int): Unit = {
    contentAtEachNode(node) := value
  }
}

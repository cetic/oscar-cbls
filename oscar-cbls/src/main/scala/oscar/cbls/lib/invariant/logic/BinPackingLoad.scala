package oscar.cbls.lib.invariant.logic

import oscar.cbls.core.propagation.Checker
import oscar.cbls.core.computation.{CBLSIntVar, ChangingIntValue, Domain, IntValue, Invariant, InvariantHelper, ShortIntNotificationTarget}

/**
  * Created by gustavbjordal on 27/05/16.
  */
case class BinPackingLoad(items: Array[IntValue], itemsizes: Array[Long]) extends Invariant with ShortIntNotificationTarget{
  for (v <- items.indices)
    registerStaticAndDynamicDependency(items(v),v)

  finishInitialization()

  private val (minVal,maxVal) = InvariantHelper.getMinMaxBoundsShort(items)
  assert(minVal==0L)
  private val maxLoad = itemsizes.foldLeft(0L)((acc,l) => acc+ l)

  val binLoad = Array.tabulate(maxVal+1)(i => CBLSIntVar(this.model,0L, Domain(0L , maxLoad), "bin_"+i))
  for(b <- binLoad){
    b.setDefiningInvariant(this)
  }

  for(i <- items.indices)
    binLoad(items(i).valueInt) :+= itemsizes(i)

  println(binLoad.mkString(", "))

  def Defines(loads:Array[CBLSIntVar]): Unit = {
    assert(loads.length == binLoad.length)
    for(i <- loads.indices)
      loads(i) <== binLoad(i)
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int): Unit = {
    binLoad(OldVal) :-= itemsizes(id)
    binLoad(NewVal) :+= itemsizes(id)
  }

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker): Unit = {
    c.check(true,Some("nothing to check, invariant is discharged"))
  }
}

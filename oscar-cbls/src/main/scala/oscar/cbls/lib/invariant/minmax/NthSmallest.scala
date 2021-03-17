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
package oscar.cbls.lib.invariant.minmax

import oscar.cbls.algo.search.LazyQuicksort
import oscar.cbls.core.computation.{ChangingIntValue, Domain, IntInvariant, IntNotificationTarget, IntValue}
import oscar.cbls.core.propagation.Checker

object  NthSmallest {
  /**
    * if smallest is true
    * maintains nth smallest value from vars
    * otherwise maintains nth biggest value from vars
    *
    * @param vars some variables
    * @param nTh the nTh value you want in the sort
    * @param smallest true to get the smallest, false to get the biggest
    */
  def apply(vars: Array[IntValue], nTh: Int, smallest: Boolean = true):NthSmallest = {
    if(nTh <= vars.length/2) {
      new NthSmallest(vars, nTh, smallest)
    }else{
      new NthSmallest(vars, vars.length - nTh - 1, !smallest)
    }
  }
}

/**
  * if smallest is true
  * maintains nth smallest value from vars
  * otherwise maintains nth biggest value from vars
  *
  * @param vars some variables
  * @param nTh the nTh value you want in the sort. should be <= vars.length/2
  * @param smallest true to get the smallest, false to get the biggest
  */
class NthSmallest(vars: Array[IntValue], nTh: Int, smallest:Boolean = true)
  extends IntInvariant(initialDomain = Domain(vars.foldLeft((Long.MaxValue,Long.MinValue))((acc, v) => (acc._1 min v.min,acc._2 max v.max))),
    initialValue = vars.foldLeft(Long.MaxValue)((acc, v) => acc min v.min))
    with IntNotificationTarget{

  require(nTh >= 0, "nthShould be positive")
  require(nTh < vars.length, "nTh should fall within the number of variables")

  require(nTh <= (vars.length/2), s"invert the invariant for efficiency reasons, nTh:$nTh should be <=${vars.length/2}")

  registerStaticAndDynamicDependencyArrayIndex(vars)
  finishInitialization()

  //if MaxInt, then this is stable
  var changeTrackSinceLastPropagation:Int = Int.MaxValue

  scheduleForPropagation()

  //the principle is that we keep output unchanged unless ome value that on one side of the percentile changes side.
  //we count the number of such changes (positive if goes above, negative if goes below)
  //exception if the changed value is the percentile
  //upon propagation, if the amount is zero, nothing to do, otherwise, we compute the percentile from scratch, unsing lazy quicksort method.
  //the signal itself is setting the counter to MaxInt
  override def notifyIntChanged(v: ChangingIntValue, id: Int, oldVal: Long, newVal: Long): Unit = {
    if (changeTrackSinceLastPropagation == Int.MaxValue) return

    val currentValue = this.newValue

    if (oldVal == currentValue){
      changeTrackSinceLastPropagation = Int.MaxValue
      scheduleForPropagation()
    }else if(oldVal < currentValue && currentValue < newVal){
      changeTrackSinceLastPropagation += 1
      scheduleForPropagation()
    }else if(currentValue  < oldVal && newVal < currentValue ){
      changeTrackSinceLastPropagation -= 1
      scheduleForPropagation()
    }//So we do not even schedule for propagation if a change does not impact percentile
  }

  override def performInvariantPropagation(): Unit = {
    if(changeTrackSinceLastPropagation == 0) return //change are compensating each other
    changeTrackSinceLastPropagation = 0

    //we use a lazy quicksort here, and hope nTh is small
    val lq = new LazyQuicksort(vars.map(_.valueInt), if (smallest) (a => a) else (a => -a))

    this := lq(nTh)
  }

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker): Unit = {

    val sortedValues:Array[Long] = this.vars.map(_.value).sorted
    if(smallest){
      c.check(sortedValues(nTh) == this.newValue,
        Some(s"smallest sortedValues:${sortedValues.mkString(",")} nTh:$nTh expected:${this.newValue}"))
    }else{
      c.check(sortedValues(vars.length - (nTh + 1)) == this.newValue,
        Some(s"biggest sortedValues:${sortedValues.mkString(",")} nTh:$nTh expected:${this.newValue}"))
    }
  }
}

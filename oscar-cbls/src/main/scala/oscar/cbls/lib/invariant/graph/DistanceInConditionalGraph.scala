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

package oscar.cbls.lib.invariant.graph

import oscar.cbls._
import oscar.cbls.algo.graph._
import oscar.cbls.core.computation.{ChangingIntValue, ChangingSetValue, Domain, IntInvariant, IntNotificationTarget, IntValue, SetNotificationTarget, SetValue, ValueWiseKey, VaryingDependencies}
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet

class DistanceInConditionalGraph(graph:ConditionalGraph,
                                 from:IntValue,
                                 to:IntValue,
                                 openConditions:SetValue,
                                 distanceIfNotConnected:Long) //must be bigger than the real distance!!
                                (underApproximatingDistance:(Int,Int) => Long
                                 = {val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true);
                                  (a:Int,b:Int) =>{
                                    underApproxDistanceMatrix(a)(b)
                                  }})
  extends IntInvariant(initialDomain = Domain(0,distanceIfNotConnected))
    with VaryingDependencies
    with SetNotificationTarget
    with IntNotificationTarget {

  registerStaticDependency(openConditions)
  registerStaticAndDynamicDependency(from)
  registerStaticAndDynamicDependency(to)

  private val key: ValueWiseKey = registerDynamicValueWiseDependency(openConditions)

  finishInitialization()

  val aStar = new RevisableAStar(graph, underApproximatingDistance)

  var listenedValues: SortedSet[Int] = SortedSet.empty

  def setListenedValueOnValueWiseKey(newListenedValues: SortedSet[Int]): Unit = {
    val toRemoveValues = listenedValues -- newListenedValues

    toRemoveValues.foreach(k => key.removeFromKey(k))

    val toAddValues = newListenedValues -- listenedValues
    toAddValues.foreach(k => key.addToKey(k))

    listenedValues = newListenedValues
  }

  //initialize the stuff
  scheduleForPropagation()

  def getPath: RevisableDistance = {
    getRevisableDistance(withPath = true)
  }

  def getRevisableDistance(withPath: Boolean): RevisableDistance = {
    val fromID = longToInt(from.value)
    val toID = longToInt(to.value)
    if (fromID == -1 || toID == -1) NeverConnected(null, null)
    else {
      val o = openConditions.value
      aStar.search(
        graph.nodes(fromID),
        graph.nodes(toID),
        condition => o contains condition,
        withPath)
    }
  }

  var fromAtLatestComputation:Long = -1
  var toAtLatestComputation:Long = -1

  def computeAffectAndAdjustValueWiseKey(): Unit = {
    //println("computeAffectAndAdjustValueWiseKey")

    val (a,b) =
      if(fromAtLatestComputation<toAtLatestComputation)
        (fromAtLatestComputation,toAtLatestComputation)
      else (toAtLatestComputation,fromAtLatestComputation)

    val fromValue = from.value
    val toValue = to.value

    val (c,d) =
      if(fromValue < toValue) (fromValue,toValue)
      else (toValue,fromValue)

    if(a == c && b == d){
      //We are essentially asking for the same path,
      // maybe the values were reversed,
      // in that case,since it is symmetric, we do nothing
      return
    }else{
      fromAtLatestComputation = fromValue
      toAtLatestComputation = toValue
    }

    getRevisableDistance(withPath = false)
    match {
      case d@Distance(_, _, distance: Long, requiredConditions, unlockingConditions, _) =>
        //println("Distance")

        setListenedValueOnValueWiseKey(requiredConditions ++ unlockingConditions)
        this := distance

      case n@NeverConnected(_, _) =>
        //println("NeverConnected")
        //these two nodes will never be connected, so nothing to listen in the condition changes

        setListenedValueOnValueWiseKey(SortedSet.empty[Int])
        this := distanceIfNotConnected

      case n@NotConnected(_, _, unlockingConditions) =>
        //println("NotConnected")
        //println("computeAffectAndAdjustValueWiseKey" + n)
        setListenedValueOnValueWiseKey(unlockingConditions)

        this := distanceIfNotConnected
    }
  }

  override def notifySetChanges(v: ChangingSetValue,
                                d: Int,
                                addedValues: Iterable[Int],
                                removedValues: Iterable[Int],
                                oldValue: SortedSet[Int],
                                newValue: SortedSet[Int]): Unit = {

    //this looks a bit drastic,
    // however, we are in a value-wise context;
    // so this method is only called
    // when something happened to the graph
    // that requires the path to be re-computed
    scheduleForPropagation()

    fromAtLatestComputation = -1 //we kil the guard on node swap to ensure computation is performed
  }

  override def notifyIntChanged(v: ChangingIntValue, id: Int, oldVal: Long, newVal: Long): Unit = {
    //we changed the from or the to, so path recomputation is needed
    scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    computeAffectAndAdjustValueWiseKey()
  }

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(checker: Checker): Unit = {

    //We rely on the existing Astar, but call it twice.

    val fromID = longToInt(from.value)
    val toID = longToInt(to.value)

    if (fromID == -1 || toID == -1) {
      assert(this.value == distanceIfNotConnected)
    } else {

      val fwd = aStar.search(
        graph.nodes(fromID),
        graph.nodes(toID),
        {
          val o = openConditions.value; condition => o contains condition
        }, false)

      val bwt = aStar.search(
        graph.nodes(toID),
        graph.nodes(fromID),
        {
          val o = openConditions.value; condition => o contains condition
        }, false)

      (fwd, bwt) match {
        case (Distance(a, b, distance1, _, _, _), Distance(c, d, distance2, _, _, _)) =>
          assert(this.value == distance1)
          assert(distance1 == distance2)

        case (NeverConnected(a, b), NeverConnected(c, d)) =>
          assert(this.value == distanceIfNotConnected)

       case (NotConnected(a, b, _), NotConnected(c, d, _)) =>
          //println("computeAffectAndAdjustValueWiseKey" + n)
          assert(this.value == distanceIfNotConnected)
        case _ => throw new Error("disagreeing aStar")
      }
    }
  }
}

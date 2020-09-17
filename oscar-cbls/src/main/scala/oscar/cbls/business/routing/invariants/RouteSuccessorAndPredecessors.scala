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
package oscar.cbls.business.routing.invariants

import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.business.routing.model.RoutingConventionMethods
import oscar.cbls.core.computation.{CBLSIntVar, ChangingSeqValue, Invariant, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint}
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet

object RouteSuccessorAndPredecessors{
  def apply(routes:ChangingSeqValue,
            v:Int,
            defaultWhenNotInSequence:Int):(Array[CBLSIntVar],Array[CBLSIntVar]) = {
    val n = routes.maxValue + 1
    val model = routes.model
    val successorVars = Array.tabulate(n)(node =>  CBLSIntVar(model,defaultWhenNotInSequence,name="successor of node" + node))
    val predecessorVars = Array.tabulate(n)(node =>  CBLSIntVar(model,defaultWhenNotInSequence,name="predecessor of node" + node))

    new RouteSuccessorAndPredecessors(routes, v, successorVars,predecessorVars,defaultWhenNotInSequence)

    (successorVars,predecessorVars)
  }
}

/**
 * This array will maintain successorValues and successorValues to be the successor and predecessors of nodes in the route
 * it comes handy to spcify constraints or objectives. But it really slows down your model
 * @param routes a sequece representing the route, according to the routing convention
 * @param v the number of vehicles
 * @param successorValues an array of CBLSIntVar that are maintained to map node to the next reached node, according to the sequence
 * @param predecessorValues an array of CBLSIntVar that are maintained to map node to the prev reached node, according to the sequence
 * @param defaultWhenNotInSequence the value to put in the two arrays for nodes that  are not in the sequence
 */
class RouteSuccessorAndPredecessors(routes:ChangingSeqValue,
                     v:Int,
                     successorValues:Array[CBLSIntVar],
                     predecessorValues:Array[CBLSIntVar],
                     defaultWhenNotInSequence:Int)
  extends Invariant() with SeqNotificationTarget {

  val n = routes.maxValue + 1
  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  for(i <- successorValues) i.setDefiningInvariant(this)
  for(i <- predecessorValues) i.setDefiningInvariant(this)

  computeAllFromScratch(routes.value)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {

    val startValuesOfImpactedZone = computeStartValuesOfImpactedZone(changes:SeqUpdate)
    startValuesOfImpactedZone match{
      case None =>  computeAllFromScratch(changes.newValue)
      case Some(startUpdateValues) =>
        val startUpdateExplorersAndVal = startUpdateValues.toList.map(value => (value,changes.newValue.explorerAtAnyOccurrence(value)))
        val startUpdateExplorersAndValSortedByPos =
          startUpdateExplorersAndVal.sortBy(valueAndExplorer => valueAndExplorer._2 match{
            case None => -valueAndExplorer._1
            case Some(explorer) => explorer.position})

        startUpdateExplorersAndValSortedByPos.foreach(startValueAndExplorer =>
          updateStartFrom(startValueAndExplorer._1,startValueAndExplorer._2,changes.newValue))
    }
  }

  /**
   *
   * @param changes
   * @return the set of values that require updating for next and prev
   */
  def computeStartValuesOfImpactedZone(changes:SeqUpdate):Option[SortedSet[Int]] = {
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        computeStartValuesOfImpactedZone(prev) match{
          case None => None
          case Some(startsOfImpactedZone) =>
            Some(startsOfImpactedZone + value
              + RoutingConventionMethods.routingPredVal2Val(value,s.newValue,v)
              + RoutingConventionMethods.routingSuccVal2Val(value,s.newValue,v))
        }

      case m@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        computeStartValuesOfImpactedZone(prev) match{
          case None => None
          case Some(startsOfImpactedZone) => Some(
            startsOfImpactedZone +
              m.fromValue + RoutingConventionMethods.routingPredPos2Val(fromIncluded,prev.newValue,v) + RoutingConventionMethods.routingSuccPos2Val(fromIncluded,prev.newValue,v) +
              m.toValue + RoutingConventionMethods.routingPredPos2Val(toIncluded,prev.newValue,v) + RoutingConventionMethods.routingSuccPos2Val(toIncluded,prev.newValue,v) +
              m.afterValue+ RoutingConventionMethods.routingPredPos2Val(after,prev.newValue,v) + RoutingConventionMethods.routingSuccPos2Val(after,prev.newValue,v))
        }

      case r@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        val removedValue = r.removedValue
        computeStartValuesOfImpactedZone(prev) match{
          case None => None
          case Some(startsOfImpactedZone) => Some(
            startsOfImpactedZone +
              removedValue +
              RoutingConventionMethods.routingPredPos2Val(position,prev.newValue,v)
              + RoutingConventionMethods.routingSuccPos2Val(position,prev.newValue,v))
        }

      case SeqUpdateLastNotified(value) =>
        require (value quickEquals routes.value)
        Some(SortedSet.empty[Int]) //we are starting from the previous value
      case SeqUpdateAssign(value : IntSequence) =>
        None //impossible to go incremental
      case SeqUpdateDefineCheckpoint(prev:SeqUpdate,_,_) =>
        computeStartValuesOfImpactedZone(prev)
      case u@SeqUpdateRollBackToCheckpoint(_,_) =>
        computeStartValuesOfImpactedZone(u.howToRollBack)
    }
  }

  def computeAllFromScratch(seq:IntSequence): Unit ={
    successorValues.foreach(node => node := defaultWhenNotInSequence)
    predecessorValues.foreach(node => node := defaultWhenNotInSequence)
    var explorer = seq.explorerAtPosition(0).head
    while(explorer.next match{
      case None =>
        successorValues(explorer.value) := v-1
        predecessorValues(v-1) := explorer.value
        false
      case Some(next) =>
        if(next.value < v){
          successorValues(explorer.value) := next.value - 1
          predecessorValues(next.value - 1) := explorer.value
        }else{
          successorValues(explorer.value) := next.value
          predecessorValues(next.value) := explorer.value
        }
        explorer = next
        true
    }){}
  }

  def updateStartFrom(startValue:Int,startExplorerOpt:Option[IntSequenceExplorer],seq:IntSequence): Unit ={
    startExplorerOpt match{
      case None =>
        successorValues(startValue) := defaultWhenNotInSequence
        predecessorValues(startValue) := defaultWhenNotInSequence
      case Some(startExplorer) =>
        var explorer = startExplorer
        while(explorer.next match{
          case None =>
            successorValues(explorer.value) := v-1
            predecessorValues(v-1) := explorer.value
            false
          case Some(next) =>
            val newValueForSuccValue =
              if(next.value < v) next.value - 1
              else next.value
            if(successorValues(explorer.value).newValue != newValueForSuccValue){
              successorValues(explorer.value) := newValueForSuccValue
              predecessorValues(newValueForSuccValue) := explorer.value
              explorer = next
              true
            }else false
        }){}
    }
  }

  def computeSuccessorsFromScratchNoAffect(seq:IntSequence):Array[Int] = {
    val successorValues = Array.fill(n)(defaultWhenNotInSequence)

    var explorer = seq.explorerAtPosition(0).head
    while(explorer.next match{
      case None =>
        successorValues(explorer.value) = v-1
        false
      case Some(next) =>
        if(next.value < v){
          successorValues(explorer.value) = next.value - 1
        }else{
          successorValues(explorer.value) = next.value
        }
        explorer = next
        true
    }){}
    successorValues
  }

  override def checkInternals(c : Checker): Unit ={
    require(routes.value quickEquals routes.newValue)
    val fromScratch = computeSuccessorsFromScratchNoAffect(routes.newValue)
    for(node <- 0 until n){
      c.check(successorValues(node).newValue == fromScratch(node),
        Some(s"error on next for node $node: ${successorValues(node).newValue} should== ${fromScratch(node)}"))

      if(fromScratch(node)== defaultWhenNotInSequence){
        c.check(predecessorValues(node).newValue == defaultWhenNotInSequence,
          Some(s"error on predecessor for node $node it is not routed, but got ${predecessorValues(node).newValue}"))
      }else {
        c.check(predecessorValues(fromScratch(node)).newValue == node,
          Some(s"error on predecessor for node $node successor from scratch:${fromScratch(node)} predecessor of this is: ${predecessorValues(fromScratch(node)).newValue}seq:${routes.value}"))
      }
    }
  }
}

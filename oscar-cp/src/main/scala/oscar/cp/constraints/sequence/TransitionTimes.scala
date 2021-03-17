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

package oscar.cp.constraints.sequence

import java.util

import oscar.algo._
import oscar.algo.reversible._
import oscar.cp.core.variables._
import oscar.cp.core._

import scala.collection.mutable

abstract class TransitionTimes(
                 sequence: CPSeqVar,
                 starts: Array[CPIntVar], //Starts of activities
                 durations: Array[CPIntVar], //Durations of activities
                 ends: Array[CPIntVar], //Ends of activities
                 transitions: Array[Array[Int]] //Transition times
               ) extends Constraint(sequence.store) {
  protected var maxPathCheckDepth = 1
  protected var filterThreshold = 10
  protected var useCache = true

  def setPathCheckDepth(depth: Int): Unit = maxPathCheckDepth = depth
  def setFilterThreshold(t: Int): Unit = filterThreshold = t
  def setCache(use: Boolean): Unit = useCache = use

  override def associatedVars(): Iterable[CPVar] = Array(sequence) ++ starts ++ durations ++ ends

  priorityL2 = CPStore.MinPriorityL2

  override def setup(l: CPPropagStrength): Unit = {
    //Setting up propagation strength:
    l match {
      case CPPropagStrength.Weak =>
        maxPathCheckDepth = 1
        filterThreshold = 10
      case CPPropagStrength.Medium =>
        maxPathCheckDepth = 3
        filterThreshold = 20
      case CPPropagStrength.Strong =>
        maxPathCheckDepth = 6
        filterThreshold = Int.MaxValue
      case _ =>
        maxPathCheckDepth = 3
        filterThreshold = 20
    }

    propagate() //Initial propagation

    //Setting up propagation calls:
    //TODO: Improve efficiency, call specific filtering depending on variables changed
    for (i <- starts.indices) if (sequence.isPossibleOrMember(i)) {
      starts(i).callPropagateWhenBoundsChange(this)
      durations(i).callPropagateWhenBoundsChange(this)
      ends(i).callPropagateWhenBoundsChange(this)
    }
    sequence.callPropagateWhenDomainChanges(this)
  }
}

class TransitionTimesHead(
                           sequence: CPHeadSeqVar,
                           starts: Array[CPIntVar], //Starts of activities
                           durations: Array[CPIntVar], //Durations of activities
                           ends: Array[CPIntVar], //Ends of activities
                           transitions: Array[Array[Int]] //Transition times
               ) extends TransitionTimes(sequence, starts, durations, ends, transitions) {

  private val currentTime = new ReversibleInt(sequence.store, starts.map(_.min).min)

  private val feasiblePathMap = new mutable.HashMap[(Int, util.BitSet), (Int, Int)]() //Used for path feasibility memoization

  override def propagate(): Unit = {
    updateAppendedStarts()
    updateAppendedEnds(true)
    updatePossibleTimeWindows()
    feasiblePathMap.clear()
    failIfNoPathBetweenMandatory()
    filteringPossibleElemsDueToMandatories()
    branchNextIfNoAlternative()
  }

  /**
    * Updating starts of time windows for appended activities
    */
  private def updateAppendedStarts(visits: Iterator[Int] = sequence.allMembers.iterator): Unit = {
    if (visits.isEmpty) return
    var prevVisit = visits.next()
    var time = ends(prevVisit).min
    for (nextVisit <- visits) {
      starts(nextVisit).updateMin(math.max(time + transitions(prevVisit)(nextVisit), starts(nextVisit).min))
      time = ends(nextVisit).min
      prevVisit = nextVisit
    }
    currentTime.setValue(time)
  }

  /**
    * Updating ends of time windows for appended activities
    */
  private def updateAppendedEnds(checkToStart: Boolean = false): Unit = {
    val visits = sequence.allMembers
    if (visits.isEmpty) return
    var nextIdx = visits.length - 1
    var time = starts(visits(nextIdx)).max
    var prevIdx = nextIdx - 1
    while (prevIdx > 0) {
      time = math.min(time - transitions(visits(prevIdx))(visits(nextIdx)), ends(visits(prevIdx)).max)
      if (time >= ends(visits(prevIdx)).max && !checkToStart) return
      ends(visits(prevIdx)).updateMax(time)
      time = starts(visits(prevIdx)).max
      nextIdx = prevIdx
      prevIdx -= 1
    }
  }

  /**
    * Updating time windows for possible activities
    */
  private def updatePossibleTimeWindows(last: Int = sequence.lastAppended, time: Int = currentTime.value): Unit = {
    for (act <- sequence.allPossibleOrRequired) {
      val actEst = math.max(starts(act).min, time + (if (last == -1) 0 else transitions(last)(act)))
      val actEct = actEst + durations(act).min
      try {
        starts(act).updateMin(actEst)
        ends(act).updateMin(actEct)
      } catch {
        case _: Inconsistency => sequence.excludes(act) //If failure when updating time windows: excluding activity from sequence
      }
    }
  }

  /**
    * Checks that a feasible path exists between all mandatory activities
    */
  private def failIfNoPathBetweenMandatory(): Unit = {
    val acts = new util.BitSet(starts.length)
    for (a <- sequence.allRequiredNotMember) acts.set(a)
    if (!feasiblePath(sequence.lastAppended, currentTime.value, acts, starts.length, maxPathCheckDepth)) throw Inconsistency
  }

  private def filteringPossibleElemsDueToMandatories(): Unit = for (actId <- sequence.allAppendableNext) checkPathFeasibility(actId)

  /**
    * Checks that a feasible path exists between the activity and all mandatory activities
    */
  private def checkPathFeasibility(actId: Int): Unit = {
    val acts = new util.BitSet(starts.length)
    for (a <- sequence.allRequiredNotMember) acts.set(a)
    acts.set(actId)
    if (!feasiblePath(sequence.lastAppended, currentTime.value, acts, actId, maxPathCheckDepth)) sequence.excludes(actId)
  }

  //Forget allows to ignore memoization if the act is part of the set.
  private def feasiblePath(l: Int, t: Int, acts: util.BitSet, forget: Int = starts.length, depth: Int = maxPathCheckDepth): Boolean = {
    if (acts.isEmpty) return true //if set empty: A path exists

    //Looking in memoized data:
    val (tf, ti) = feasiblePathMap.getOrElse((l, acts), (Int.MinValue, Int.MaxValue))
    if (t <= tf) return true
    else if (t >= ti) return false

    val actIds = acts.stream().toArray

    //Checking that each activity can be reached:
    for (a <- actIds) {
      val arrival = math.max(t + (if (l == -1) 0 else transitions(l)(a)), starts(a).min)
      val departure = arrival + durations(a).min
      if (arrival > starts(a).max || departure > ends(a).max) {
        if (!acts.get(forget) && t < ti) feasiblePathMap((l, acts)) = (tf, t)
        return false
      }
    }

    //Branching on activities if max depth not reached:
    if (depth > 0) {
      for (a <- actIds.sortBy(ends(_).min)) {
        val departure = math.max(t + (if (l == -1) 0 else transitions(l)(a)), starts(a).min) + durations(a).min
        acts.clear(a)
        val feasible = feasiblePath(a, departure, acts, forget, depth - 1)
        acts.set(a)
        if (feasible) {
          if (!acts.get(forget) && t > tf) feasiblePathMap((l, acts)) = (t, ti)
          return true
        }
      }

      if (!acts.get(forget) && t < ti) feasiblePathMap((l, acts)) = (tf, t)
      false

    } else true //Returning true if max depth reached
  }


  /**
    * Checks if need to visit mandatory activity next
    */
  private def branchNextIfNoAlternative(): Unit = {
    val m = sequence.allRequiredNotMember.filter(sequence.isAppendableNext).find(m => !detourPossible(m))
    if (m.nonEmpty) sequence.markAppended(m.get)
  }

  private def detourPossible(m: Int): Boolean = {
    val last = sequence.lastAppended
    for (a <- sequence.allAppendableNext.filter(_ != m)) {
      val arra = math.max(currentTime.value + (if (last == -1) 0 else transitions(last)(a)), starts(a).min)
      val depa = arra + durations(a).min
      val arrm = math.max(depa + transitions(a)(m), starts(m).min)
      val depm = arrm + durations(m).min
      if (arra <= starts(a).max && depa <= ends(a).max && arrm <= starts(m).max && depm <= ends(m).max) {
        return true
      }
    }
    false
  }
}

class TransitionTimesInsert(
                     sequence: CPInsertSeqVar,
                     starts: Array[CPIntVar], //Starts of activities
                     durations: Array[CPIntVar], //Durations of activities
                     ends: Array[CPIntVar], //Ends of activities
                     transitions: Array[Array[Int]] //Transition times
                   ) extends TransitionTimes(sequence, starts, durations, ends, transitions) {

  private val feasiblePathMap = new mutable.HashMap[(Int, Int, util.BitSet), (Int, Int)]() //Used for path feasibility memoization

  override def propagate(): Unit = {
    updateMemberStarts()
    updateMembersEnds()
    filterInsertions()
    updatePossibleTimeWindows()
    feasiblePathMap.clear()
    failIfNoPathBetweenMandatory()
    if(sequence.possibleSize < filterThreshold) filteringPossibleElemsDueToMandatories()
  }

  /**
    * Updating starts of time windows for member activities
    */
  private def updateMemberStarts(): Unit = {
    val members = sequence.allMembersIterator
    if (members.isEmpty) return
    var prevMember = members.next()
    var time = ends(prevMember).min
    for (nextMember <- members) {
      starts(nextMember).updateMin(math.max(time + transitions(prevMember)(nextMember), starts(nextMember).min))
      time = ends(nextMember).min
      prevMember = nextMember
    }
  }

  /**
    * Updating ends of time windows for member activities
    */
  private def updateMembersEnds(): Unit = {
    val members = sequence.allMembersIteratorReversed
    if (members.isEmpty) return
    var nextMember = members.next()
    var time = starts(nextMember).max
    for (prevMember <- members) {
      ends(prevMember).updateMax(math.min(time - transitions(prevMember)(nextMember), ends(prevMember).max))
      time = starts(prevMember).max
      nextMember = prevMember
    }
  }

  private def updatePossibleTimeWindows(): Unit = {
    for((elem, preds) <- sequence.currentInsertionsPerElem){
      val ea = preds.map(pred => math.max(ends(pred).min + transitions(pred)(elem), starts(elem).min)).min
      val ld = preds.map(pred => {
        val next = sequence.nextMember(pred)
        if(next == -1) ends(elem).max
        else math.min(starts(next).max - transitions(elem)(next), ends(elem).max)
      }).max
      try {
        starts(elem).updateMin(ea)
        ends(elem).updateMax(ld)
      } catch {
        case _: Inconsistency => sequence.excludes(elem) //If failure when updating time windows: excluding activity from sequence
      }
    }
  }

  /**
    * Checks that a feasible path exists between all mandatory activities
    */
  private def failIfNoPathBetweenMandatory(): Unit = {
    val acts = new util.BitSet(starts.length)
    for (a <- sequence.allRequiredNotMember) acts.set(a)
    if (!feasiblePath(acts)) throw Inconsistency
  }

  private def filteringPossibleElemsDueToMandatories(): Unit = for (actId <- sequence.allPossibleOrRequired) checkPathFeasibility(actId)

  /**
    * Checks that a feasible path exists between the activity and all mandatory activities
    */
  private def checkPathFeasibility(actId: Int): Unit = {
    val acts = new util.BitSet(starts.length)
    for (a <- sequence.allRequiredNotMember) acts.set(a)
    acts.set(actId)
    if (!feasiblePath(acts, actId)) sequence.excludes(actId)
  }

  private def feasiblePath(actsSet: util.BitSet, forget: Int = starts.length): Boolean ={

    def feasiblePathAux(l: Int, n: Int, t: Int, acts: util.BitSet, depth: Int): Boolean = {

      //Checking that n is still reachable:
      if(n != -1){
        val arrival = math.max(t + (if (l == -1) 0 else transitions(l)(n)), starts(n).min)
        if (arrival > starts(n).max ) return false
      }

      if (acts.isEmpty) return true //if set empty: A path exists

      //Looking in memoized data:
      val (tf, ti) = if(useCache) feasiblePathMap.getOrElse((l, n, acts), (Int.MinValue, Int.MaxValue)) else (Int.MinValue, Int.MaxValue)
      if (t <= tf) return true
      else if (t >= ti) return false

      val actIds = acts.stream().toArray

      //Checking that each activity is compatible:
      for (a <- actIds) {
        val arrival = math.max(t + (if (l == -1) 0 else transitions(l)(a)), starts(a).min)
        val departure = arrival + durations(a).min
        if (arrival > starts(a).max || departure > ends(a).max) {
          if (useCache && !acts.get(forget) && t < ti) feasiblePathMap((l, n, acts)) = (tf, t)
          return false
        }
      }

      //If max depth not reached:
      if (depth > 0) {

        //Branching on activities:
        for (a <- actIds.filter(sequence.isInsertableAfter(_, l)).sortBy(ends(_).min)) {
          val departure = math.max(t + (if (l == -1) 0 else transitions(l)(a)), starts(a).min) + durations(a).min
          acts.clear(a)
          val feasible = feasiblePathAux(a, n, departure, acts, depth - 1)
          acts.set(a)
          if (feasible) {
            if (useCache && !acts.get(forget) && t > tf) feasiblePathMap((l, n, acts)) = (t, ti)
            return true
          }
        }

        //Branching on n:
        if(n != -1) {
          val departure = math.max(t + (if (l == -1) 0 else transitions(l)(n)), starts(n).min) + durations(n).min
          if (feasiblePathAux(n, sequence.nextMember(n), departure, acts, depth)) {
            if (useCache && !acts.get(forget) && t > tf) feasiblePathMap((l, n, acts)) = (t, ti)
            return true
          }
        }

        if (useCache && !acts.get(forget) && t < ti) feasiblePathMap((l, n, acts)) = (tf, t)
        false

      } else true //Returning true if max depth reached
    }

    feasiblePathAux(-1, sequence.nextMember(-1), 0, actsSet, maxPathCheckDepth)
  }

  /**
    * Filters possible insertion positions:
    */
  def filterInsertions(): Unit = {
    for((elem, pred) <- sequence.allCurrentInsertions) {
      val next = sequence.nextMember(pred)
      val eae = math.max(ends(pred).min + transitions(pred)(elem), starts(elem).min)
      if(next != -1 && eae + durations(elem).min + transitions(elem)(next) > starts(next).max) sequence.removeInsertion(elem, pred)
    }
  }
}

object TransitionTimes {
  def apply(
             sequence: CPHeadSeqVar,
             starts: Array[CPIntVar],
             durations: Array[CPIntVar],
             ends: Array[CPIntVar],
             transitions: Array[Array[Int]]
           ): TransitionTimes = new TransitionTimesHead(sequence, starts, durations, ends, transitions)

  def apply(
             sequence: CPInsertSeqVar,
             starts: Array[CPIntVar],
             durations: Array[CPIntVar],
             ends: Array[CPIntVar],
             transitions: Array[Array[Int]]
           ): TransitionTimes = new TransitionTimesInsert(sequence, starts, durations, ends, transitions)
}

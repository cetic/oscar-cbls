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

import oscar.algo.Inconsistency
import oscar.cp.core._
import oscar.cp.core.variables.{CPInsertSeqVar, CPSeqVar, CPVar}
import oscar.cp._

import scala.collection.mutable

/**
 * @author Charles Thomas (cftmthomas@gmail.com)
 *
 * Cumulative constraint over the elements of a sequence.
 *
 * @param sequence The sequence variable
 * @param starts starts of activities (possible elements of the sequence variable)
 * @param ends ends of activities (possible elements of the sequence variable)
 * @param loads loads of activities
 * @param maxCapacity max capacity of the resource
 * @param minCapacity min capacity of the resource
 */
abstract class Cumulative(
                           sequence: CPSeqVar,
                           starts: Array[Int], // starts of activities
                           ends: Array[Int], // ends of activities
                           loads: Array[CPIntVar], // loads of activities
                           maxCapacity: CPIntVar, // maximum capacity of the available resource
                           minCapacity: CPIntVar // minimum capacity of the available resource
                         ) extends Constraint(sequence.store) {

  protected var filterActive = true

  def setFilterActive(active: Boolean): Unit = filterActive = active

  override def associatedVars(): Iterable[CPVar] = Array(sequence) ++ loads :+ maxCapacity :+ minCapacity

  override def setup(l: CPPropagStrength): Unit = {
    for(i <- starts.indices) sequence.store.add(Dependency(sequence, Set(starts(i), ends(i)))) //Posting dependency constraints

    sequence.callPropagateWhenDomainChanges(this)
    for(i <- loads.indices)loads(i).callPropagateWhenBoundsChange(this)
    maxCapacity.callPropagateWhenBoundsChange(this)
    minCapacity.callPropagateWhenBoundsChange(this)
    propagate()
  }

}

class CumulativeInsert(
                        sequence: CPInsertSeqVar,
                        starts: Array[Int], // starts of activities
                        ends: Array[Int], // ends of activities
                        loads: Array[CPIntVar], // loads of activities
                        maxCapacity: CPIntVar, // maximum capacity of the available resource
                        minCapacity: CPIntVar // minimum capacity of the available resource
                      ) extends Cumulative(sequence, starts, ends, loads, maxCapacity, minCapacity) {

  val startIndices: Map[Int, Int] = starts.zipWithIndex.toMap
  val endIndices: Map[Int, Int] = ends.zipWithIndex.toMap

  private def isStart(e: Int): Boolean = startIndices.contains(e)
  private def isEnd(e: Int): Boolean = endIndices.contains(e)
  private def indexOf(e: Int): Int = if(isStart(e)) startIndices(e) else if(isEnd(e)) endIndices(e) else -1

  private def isFullyInserted(i: Int): Boolean = sequence.isMember(starts(i)) && sequence.isMember(ends(i))
  private def isNotInserted(i: Int): Boolean = !sequence.isMember(starts(i)) && !sequence.isMember(ends(i))
  private def isPartiallyInserted(i: Int): Boolean = !isFullyInserted(i) && !isNotInserted(i)

  override def propagate(): Unit = {
    //Building load profile:
    val profile = buildProfile(maxCapacity, minCapacity)

    if(filterActive){
      //Filtering partially inserted activities
      filterPartiallyInsertedActs(profile)

      //Filtering non inserted activities:
      filterNonInsertedActs(profile)
    }
  }

  private def buildProfile(maxCapa: CPIntVar, minCapa: CPIntVar = CPIntVar(0)(sequence.store)): LoadProfile = {
    val profile = new LoadProfile(Seq((-1, 0, 0)), maxCapa, minCapa)

    //adding fully inserted activities:
    var minCurrentLoad = 0
    var maxCurrentLoad = 0
    for(current <- sequence.allMembersIterator) {
      val act = indexOf(current)
      if (act != -1 && isFullyInserted(act)) {
        if (isStart(current)) {
          minCurrentLoad += loads(act).min
          maxCurrentLoad += loads(act).max
        } else {
          minCurrentLoad -= loads(act).min
          maxCurrentLoad -= loads(act).max
        }
      }
      profile.updateMinLoad(current, minCurrentLoad)
      profile.updateMaxLoad(current, maxCurrentLoad)
    }

    //adding partially inserted activities with start fixed:
    var activeActs = mutable.Set[Int]()
    minCurrentLoad = 0
    maxCurrentLoad = 0
    for(current <- sequence.allMembersIterator) {
      val act = indexOf(current)

      //Adding starting activity:
      if(act != -1 && isPartiallyInserted(act) && isStart(current)){
        activeActs += act
        minCurrentLoad += loads(act).min
        maxCurrentLoad += loads(act).max
      }
      profile.updateMinLoad(current, minCurrentLoad)
      profile.updateMaxLoad(current, maxCurrentLoad)

      //Ending active activities if possible:
      for(active <- activeActs.toSeq.sortBy(loads(_).max).reverse){
        if(sequence.isInsertableAfter(ends(active), current)){
          activeActs -= active
          minCurrentLoad -= loads(active).min
          maxCurrentLoad -= loads(active).max
        }
      }
    }

    //Activities not ended => Failure:
    if(activeActs.nonEmpty) throw Inconsistency

    //adding partially inserted activities with end fixed:
    activeActs.clear()
    minCurrentLoad = 0
    maxCurrentLoad = 0
    var current = -1
    do {
      //Starting active activities if possible:
      for(active <- activeActs.toSeq.sortBy(loads(_).max).reverse){
        if(sequence.isInsertableAfter(starts(active), current)){
          activeActs -= active
          minCurrentLoad -= loads(active).min
          maxCurrentLoad -= loads(active).max
        }
      }
      profile.updateMinLoad(current, minCurrentLoad)
      profile.updateMaxLoad(current, maxCurrentLoad)

      //Adding ending activity:
      val act = indexOf(current)
      if(isEnd(current) && isPartiallyInserted(act)){
        activeActs += act
        minCurrentLoad += loads(act).min
        maxCurrentLoad += loads(act).max
      }

      current = sequence.predMember(current)
    } while(current != -1)

    //Activities not ended => Failure:
    if(activeActs.nonEmpty) throw Inconsistency

    profile
  }

  private def filterPartiallyInsertedActs(profile: LoadProfile): Unit = {
    for(act <- starts.indices.filter(isPartiallyInserted)){
      if(sequence.isMember(starts(act))){
        val insertions = mutable.Set(sequence.allCurrentInsertionsFor(ends(act)).toSeq: _*)
        var current = starts(act)
        var inProfile = true
        var (minLoad, maxLoad) = profile.loadAt(current)

        while(insertions.nonEmpty && maxLoad >= minCapacity.min && minLoad <= maxCapacity.max && current != -1){
          if(insertions.contains(current)){
            inProfile = false
            insertions -= current
          }
          minLoad = if(inProfile) profile.minLoadAt(current) else profile.minLoadAt(current) + loads(act).min
          maxLoad = if(inProfile) profile.maxLoadAt(current) else profile.maxLoadAt(current) + loads(act).max
          current = sequence.nextMember(current)
        }
        for(i <- insertions) sequence.removeInsertion(ends(act), i)
      } else {
        val insertions = mutable.Set(sequence.allCurrentInsertionsFor(starts(act)).toSeq: _*)
        var current = ends(act)
        var inProfile = true
        var (minLoad, maxLoad) = profile.loadAt(current)

        while(insertions.nonEmpty && maxLoad >= minCapacity.min && minLoad <= maxCapacity.max && current != -1){
          if(insertions.contains(current)){
            inProfile = false
            insertions -= current
          }
          minLoad = if(inProfile) profile.minLoadAt(current) else profile.minLoadAt(current) + loads(act).min
          maxLoad = if(inProfile) profile.maxLoadAt(current) else profile.maxLoadAt(current) + loads(act).max
          current = sequence.predMember(current)
        }
        if(maxLoad >= minCapacity.min && minLoad <= maxCapacity.max && insertions.contains(current)) insertions -= current
        for(i <- insertions) sequence.removeInsertion(starts(act), i)
      }
    }
  }

  private def filterNonInsertedActs(profile: LoadProfile): Unit = {
    for(act <- starts.indices.filter(i => isNotInserted(i) && sequence.isPossibleOrRequired(starts(i)) && sequence.isPossibleOrRequired(ends(i)))){
      val start = starts(act)
      val end = ends(act)
      val startInsertions = mutable.Set(sequence.allCurrentInsertionsFor(starts(act)).toSeq: _*)
      val endInsertions = mutable.Set(sequence.allCurrentInsertionsFor(ends(act)).toSeq: _*)
      val currentStarts = mutable.Set[Int]()

      var current = -1
      var canClose = false
      do {
        if (sequence.isInsertableAfter(start, current)){
          currentStarts += current
          canClose = true
        }

        if (currentStarts.nonEmpty && (profile.maxLoadAt(current) + loads(act).max < minCapacity.min || profile.minLoadAt(current) + loads(act).min > maxCapacity.max)) {
          currentStarts.clear()
          canClose = false
        }

        if (sequence.isInsertableAfter(end, current) && canClose) {
          endInsertions -= current
          for (si <- currentStarts) if (startInsertions.contains(si)) startInsertions -= si
          currentStarts.clear()
        }

        current = sequence.nextMember(current)
      } while(current != -1 && startInsertions.nonEmpty && endInsertions.nonEmpty)

      for(i <- startInsertions) sequence.removeInsertion(start, i)
      for(i <- endInsertions) sequence.removeInsertion(end, i)
    }
  }
}

object Cumulative {
  def apply(
             sequence: CPInsertSeqVar,
             starts: Array[Int], // starts of activities
             ends: Array[Int], // ends of activities
             loads: Array[CPIntVar], // loads of activities
             maxCapacity: CPIntVar, // maximum capacity of the available resource
             minCapacity: CPIntVar
           ) : Cumulative =
    new CumulativeInsert(sequence, starts, ends, loads, maxCapacity, minCapacity)
}

class LoadProfile(initialProfile: Seq[(Int, Int, Int)], val maxCapa: CPIntVar, val minCapa: CPIntVar){
  private val maxLoad: mutable.Map[Int, Int] = mutable.Map[Int, Int](initialProfile.map{case (elem, maxVal, _) => (elem, maxVal)}: _*)
  private val minLoad: mutable.Map[Int, Int] = mutable.Map[Int, Int](initialProfile.map{case (elem, _, minVal) => (elem, minVal)}: _*)

  def maxLoadAt(elem: Int): Int = maxLoad.getOrElse(elem, 0)

  def minLoadAt(elem: Int): Int = minLoad.getOrElse(elem, 0)

  def loadAt(elem: Int): (Int, Int) = (minLoadAt(elem), maxLoadAt(elem))

  def updateMaxLoad(elem: Int, loadDiff: Int): Unit = {
    val newLoad = maxLoadAt(elem) + loadDiff
    if(newLoad < minCapa.min) throw Inconsistency
    maxLoad += elem -> newLoad
  }

  def updateMinLoad(elem: Int, loadDiff: Int): Unit = {
    val newLoad = minLoadAt(elem) + loadDiff
    if(newLoad > maxCapa.max) throw Inconsistency
    minLoad += elem -> newLoad
  }
}

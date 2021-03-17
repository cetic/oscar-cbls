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
 *******************************************************************************/

package oscar.cp.core.variables

import oscar.algo.Inconsistency
import oscar.algo.reversible._
import oscar.cp.core._
import oscar.cp.core.delta._
import oscar.cp.core.watcher._

import scala.collection.mutable

class CPHeadSeqVar(
                    override val store: CPStore,
                    size: Int,
                    val name: String = "HEAD_SEQUENCE_VAR"
                  ) extends CPSeqVar {

  private val elems = Array.tabulate(size)(i => i)
  private val positions = Array.tabulate(size)(i => i)

  //Set domain:
  private val setDomain: CPSetVar = CPSetVar(elems.toSet)(store)

  //Sequence domain:
  private val appendedEnd: ReversibleInt = new ReversibleInt(store, 0) //end index of appended elements (excl.)

  //Candidates domain:
  private val delayed: ReversiblePointer[Set[Int]] = ReversiblePointer[Set[Int]](store, Set[Int]())
  private val appendableNext: mutable.Set[Int] = mutable.Set()
  private val appendableNextFilters: ReversibleArrayStack[mutable.Set[Int] => Unit] = new ReversibleArrayStack(store)
  private var resetAppendableNextFlag = true

  val onDomainL2 = new WatcherListL2(store)

  store.onPop {
    resetAppendableNextFlag = true
  }

  def isBound: Boolean = allPossibleOrRequired.isEmpty || allAppendableNext.isEmpty

  def inDomain(elem: Int): Boolean = positions.contains(elem)

  def cardinality: CPIntVar = setDomain.card


  //Set domain:

  def isPossibleOrRequired(elem: Int): Boolean = setDomain.isPossible(elem) && !isMember(elem)

  def isPossibleOrMember(elem: Int): Boolean = setDomain.isPossible(elem)

  def allPossibleOrRequired: Iterable[Int] = setDomain.possibleSet().filter(isPossibleOrRequired)

  def allPossibleOrMember: Iterable[Int] = setDomain.possibleSet().filter(isPossibleOrRequired)

  def isRequiredNotMember(elem: Int): Boolean = setDomain.isRequired(elem) && !isMember(elem)

  def isRequired(elem: Int): Boolean = setDomain.isRequired(elem)

  def allRequiredNotMember: Iterable[Int] = setDomain.requiredSet().filterNot(isMember)

  def allRequired: Iterable[Int] = setDomain.requiredSet()

  def requires(elem: Int): Unit = {
    if (isRequiredNotMember(elem)) return
    if (isExcluded(elem)) throw Inconsistency //trying to set impossible elem as required => inconsitency
    resetAppendableNextFlag = true
    setDomain.requires(elem)
  }

  def isExcluded(elem: Int): Boolean = !setDomain.isPossible(elem)

  def allExcluded: Iterable[Int] = elems.filter(isExcluded)

  def excludes(elem: Int): Unit = {
    if (isExcluded(elem)) return
    if (isRequiredNotMember(elem) || isMember(elem)) throw Inconsistency
    resetAppendableNextFlag = true
    setDomain.excludes(elem)
  }

  def excludeAllPossibles(): Unit = {
    if (allRequiredNotMember.nonEmpty) throw Inconsistency //Cannot bind variable if required elems remaining
    setDomain.excludesAll()
  }


  //Sequence domain:

  def getElemAt(pos: Int): Int = if(pos >= 0 && pos < length) elems(pos) else -1

  def getPosition(elem: Int): Int = if(isMember(elem)) positions(elem) else -1

  def length: Int = appendedEnd.value

  override def isEmpty: Boolean = length == 0

  def isMember(elem: Int): Boolean = positions(elem) < appendedEnd.value

  def allMembers: Seq[Int] = elems.take(appendedEnd.value)

  /**
   * @return the last element appended, -1 if no elem is appended
   */
  def lastAppended: Int = if (isEmpty) -1 else elems(appendedEnd.value - 1)

  /**
   * appends the element and propagates. Should be used for branching.
   */
  def append(elem: Int): Unit = {
    markAppended(elem)
    store.propagate()
  }

  /**
   * appends the element. Should be directly used by constraints.
   */
  def markAppended(elem: Int): Unit = {
    if (isMember(elem)) return
    if (!isAppendableNext(elem)) throw Inconsistency
    //    println("appending: " + elem)

    resetAppendableNextFlag = true
    onDomainL2.enqueue() //AC3 notification

    setDomain.requires(elem)

    //Setting element as appended
    val pos = positions(elem)
    val elem2 = elems(appendedEnd.value)
    elems(pos) = elem2
    elems(appendedEnd.value) = elem
    positions(elem) = appendedEnd.value
    positions(elem2) = pos
    appendedEnd.incr()

    clearDelayed() //Clearing delayed
    if (allAppendableNext.isEmpty && !isBound) throw Inconsistency
  }


  //Candidates domain:

  /**
   * @return true if the element is delayed (must not be appended now)
   */
  def isDelayed(elem: Int): Boolean = isPossibleOrRequired(elem) && delayed.value.contains(elem)

  /**
   * @return all the elements delayed
   */
  def allDelayed: Iterable[Int] = delayed.value.filter(isPossibleOrRequired)

  /**
   * @return true if the element is appendable (can be appended next)
   */
  def isAppendableNext(elem: Int): Boolean = {
    if (resetAppendableNextFlag) {
      resetAppendableNext()
      resetAppendableNextFlag = false
    }
    appendableNext.contains(elem)
  }

  /**
   * @return all the elements that can be appended next in the sequence
   */
  def allAppendableNext: Set[Int] = {
    if (resetAppendableNextFlag) {
      resetAppendableNext()
      resetAppendableNextFlag = false
    }
    appendableNext.toSet
  }

  /**
   * Delays elem: remove elem from appendable next
   */
  def delay(elem: Int): Unit = {
    if (!isAppendableNext(elem)) return
    if (allAppendableNext.size <= 1) throw Inconsistency //If only delayed elem remaining in appendable next => Inconsistency
    delayed.setValue(delayed.value + elem)
    resetAppendableNextFlag = true
  }

  /**
   * Clears delayed elems
   */
  def clearDelayed(): Unit = {
    if (delayed.value.nonEmpty) {
      delayed.setValue(Set[Int]())
      resetAppendableNextFlag = true
    }
  }

  private def resetAppendableNext(): Unit = {
    appendableNext.clear()
    appendableNext ++= allPossibleOrRequired
    if (delayed.value.nonEmpty) { //Filtering out delayed elems
      for (elem <- delayed.value)
        if (appendableNext.contains(elem)) appendableNext.remove(elem)
    }
    for (filter <- appendableNextFilters) filter(appendableNext) //Calling registered filters
    //    println("appendableNext: " + appendableNext.mkString(", "))
  }


  //Delta and propagation methods:

  def callPropagateWhenDomainChanges(c: Constraint): Unit = {
    onDomainL2.register(c)
    setDomain.callPropagateWhenDomainChanges(c)
  }

  def callPropagateOnChangesWithDelta(c: Constraint): DeltaSeqVar = {
    val snap = delta(c)
    onDomainL2.register(c)
    setDomain.callPropagateWhenDomainChanges(c)
    snap
  }

  def delta(constraint: Constraint, id: Int = 0): DeltaSeqVar = {
    val delta = DeltaSeqVar(this, setDomain.delta(constraint, id), id)
    constraint.registerDelta(delta)
    delta
  }

  def changed(deltaSeqVar: DeltaSeqVar): Boolean = membersChanged(deltaSeqVar) || setDomain.changed(deltaSeqVar.asInstanceOf[DeltaSetBasedSeqVar].deltaSet)

  def deltaMembers(seqVar: DeltaSeqVar): Iterator[Int] = elems.slice(seqVar.oldSizeMember, length).iterator

  def filterWhenDomainChangesWithDelta(idempotent: Boolean = false, priority: Int = CPStore.MaxPriorityL2 - 2)(filter: DeltaSeqVar => Unit)(implicit constraint: Constraint): DeltaSeqVar = {
    val propagator = new PropagatorSeqVar(this, 0, filter)
    propagator.idempotent = idempotent
    propagator.priorityL2 = priority
    callPropagateWhenDomainChanges(propagator)
    propagator.snapshot
  }

  def filterAppendableNextWhenReset(filter: mutable.Set[Int] => Unit)(implicit constraint: Constraint): Unit = {
    appendableNextFilters.push(filter)
  }
}

object CPHeadSeqVar {
  def apply(size: Int)(implicit store: CPStore): CPHeadSeqVar = new CPHeadSeqVar(store, size)

  def apply(elemIds: Seq[Int])(implicit store: CPStore): CPHeadSeqVar = {
    val size = elemIds.max
    val seqVar = new CPHeadSeqVar(store, size)
    for (elem <- 0 until size if !elemIds.contains(elem)) {
      seqVar.excludes(elem)
    }
    seqVar
  }
}



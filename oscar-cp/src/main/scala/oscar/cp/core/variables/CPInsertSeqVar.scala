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

import oscar.algo._
import oscar.algo.reversible._
import oscar.cp.core.watcher._
import oscar.cp.core._
import oscar.cp.core.delta._

import scala.collection.mutable

class CPInsertSeqVar(
                      override val store: CPStore,
                      size: Int,
                      val name: String = "INSERT_SEQUENCE_VAR"
                  ) extends CPSeqVar {

  private val nullElem = size //Dummy element
  private val elems = 0 until size

  //Set Domain:
  private val setDomain: CPSetVar = new CPSetVar(store, 0, size-1)

  //Sequence domain:
  private val successor: Array[ReversibleInt] = Array.tabulate(size+1)(i => new ReversibleInt(store, i))
  private val predecessor: Array[ReversibleInt] = Array.tabulate(size+1)(i => new ReversibleInt(store, i))
  private val currentLength = new ReversibleInt(store, 0)

  //Insertions domain:
  private val insertions: Array[ReversibleSparseSet] = Array.fill(size)(new ReversibleSparseSet(store, -1, size-1))
  for(i <- insertions.indices) insertions(i).removeValue(i)

  val onDomainL2 = new WatcherListL2(store)

  //Cache for current insertions:
  private val insertionsUpToDate: Array[Boolean] = Array.fill(insertions.length)(false)
  private val currentInsertions: Array[mutable.Set[Int]] = Array.tabulate(insertions.length)(i => mutable.Set(insertions(i).toSeq: _*))

  private def markInsertionsOutOfDate(): Unit = insertionsUpToDate.indices.foreach(insertionsUpToDate(_) = false)

  private def addNewCurrentInsertionsFor(p: Int): Unit = {
    for(e <- allPossibleOrRequired) if(insertions(e).hasValue(p)) currentInsertions(e) += p
  }

  store.onPop(markInsertionsOutOfDate())

  def isBound: Boolean = allPossibleOrRequired.isEmpty && currentLength.value == setDomain.requiredSize

  def inDomain(elem: Int): Boolean = elem >= 0 && elem < size

  def cardinality: CPIntVar = setDomain.card


  //Set domain:

  def isPossibleOrRequired(elem: Int): Boolean = setDomain.isPossible(elem) && !isMember(elem)

  def isPossibleOrMember(elem: Int): Boolean = setDomain.isPossible(elem)

  def allPossibleOrRequired: Iterable[Int] = setDomain.possibleSet().filter(isPossibleOrRequired)

  def allPossibleOrMember: Iterable[Int] = setDomain.possibleSet()

  def possibleSize: Int = setDomain.possibleSize

  def isRequiredNotMember(elem: Int): Boolean = setDomain.isRequired(elem) && !isMember(elem)

  def isRequired(elem: Int): Boolean = setDomain.isRequired(elem)

  def allRequiredNotMember: Iterable[Int] = setDomain.requiredSet().filterNot(isMember)

  def allRequired: Iterable[Int] = setDomain.requiredSet()

  def requiredSize: Int = setDomain.requiredSize

  def requires(elem: Int): Unit = {
    if (isRequiredNotMember(elem)) return
    if (isExcluded(elem)) throw Inconsistency //trying to set impossible elem as required => inconsitency
    setDomain.requires(elem)
  }

  def isExcluded(elem: Int): Boolean = !setDomain.isPossible(elem)

  def allExcluded: Iterable[Int] = elems.filter(isExcluded)

  def excludes(elem: Int): Unit = {
    if (isExcluded(elem)) return
    if (isRequiredNotMember(elem) || isMember(elem)) throw Inconsistency
    setDomain.excludes(elem)
    clearInsertionsFor(elem)
    removeInsertionsAfter(elem)
  }

  def excludeAllPossibles(): Unit = {
    if (allRequiredNotMember.nonEmpty) throw Inconsistency //Cannot bind variable if required elems remaining
    setDomain.excludesAll()
  }


  //Sequence domain methods:

  def length: Int = currentLength.value

  override def isEmpty: Boolean = length == 0

  def isMember(elem: Int): Boolean = elem == -1 || successor(elem).value != elem

  /**
    * @return the current sequence of member elements as an iterator
    */
  def allMembersIterator: Iterator[Int] = {
    var current = successor(nullElem).value
    new Iterator[Int] {
      def next(): Int = {
        val elem = current
        current = successor(current)
        elem
      }

      def hasNext: Boolean = {
        current != nullElem
      }
    }
  }

  /**
    * @return the current sequence of member elements as an iterator in reversed order
    */
  def allMembersIteratorReversed: Iterator[Int] = {
    var current = predecessor(nullElem).value
    new Iterator[Int] {
      def next(): Int = {
        val elem = current
        current = predecessor(current)
        elem
      }
      def hasNext: Boolean = {
        current != nullElem
      }
    }
  }

  def allMembers: Seq[Int] = allMembersIterator.toSeq

  def nextMember(elem: Int): Int = {
    if(!isMember(elem)) throw new Exception("Elem is not a member")
    val next = if(elem == -1) successor(nullElem).value else successor(elem).value
    if(next == nullElem) -1 else next
  }

  def predMember(elem: Int): Int = {
    if(!isMember(elem)) throw new Exception("Elem is not a member")
    val pred = if(elem == -1) predecessor(nullElem).value else predecessor(elem).value
    if(pred == nullElem) -1 else pred
  }

  /**
    * inserts the element after specified predecessor and propagates. Should be used for branching.
    */
  def insertAfter(elem: Int, pred: Int): Unit = {
    markInsertedAfter(elem, pred)
    store.propagate()
  }

  /**
    * inserts the element after specified predecessor. Should be directly used by constraints.
    */
  def markInsertedAfter(elem: Int, pred: Int): Unit = {
    if (isMember(elem)) return
    if (!isCurrentlyInsertableAfter(elem, pred)) throw Inconsistency

    onDomainL2.enqueue() //AC3 notification

    setDomain.requires(elem)

    //Inserting element at given position
    val predElem = if(pred == -1) nullElem else pred
    val next = successor(predElem).value

    successor(elem).setValue(next)
    successor(predElem).setValue(elem)
    predecessor(elem).setValue(predElem)
    predecessor(next).setValue(elem)

    currentLength.incr()
    clearInsertionsFor(elem)
    addNewCurrentInsertionsFor(elem)
  }

  /**
    * inserts the elements after specified predecessor and propagates. Should be used for branching.
    */
  def insertAfter(elemsToInsert: Seq[Int], pred: Int): Unit = {
    markInsertedAfter(elemsToInsert, pred)
    store.propagate()
  }

  /**
    * inserts the elements after specified predecessor. Should be directly used by constraints.
    */
  def markInsertedAfter(elemsToInsert: Seq[Int], pred: Int): Unit = {
    var currPred = if(pred == -1) nullElem else pred
    val next = successor(currPred).value

    onDomainL2.enqueue() //AC3 notification

    for(elem <- elemsToInsert){
//      println("inserting " + elem + " after " + elems(iPrev))
      setDomain.requires(elem)

      //Inserting element at given position

      if(successor(currPred).value != elem){
        successor(elem).setValue(next)
        successor(currPred).setValue(elem)
        predecessor(elem).setValue(currPred)
        predecessor(next).setValue(elem)

        currentLength.incr()
        clearInsertionsFor(elem)
      }
      currPred = elem
    }
    markInsertionsOutOfDate()
  }

  /**
   * inserts the element before specified successor and propagates. Should be used for branching.
   */
  def insertBefore(elem: Int, succ: Int): Unit = insertAfter(elem, predMember(succ))

  /**
   * inserts the elements before specified successor and propagates. Should be used for branching.
   */
  def insertBefore(elems: Seq[Int], succ: Int): Unit = insertAfter(elems, predMember(succ))

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
  def markAppended(elem: Int): Unit = markInsertedAfter(elem, predecessor(nullElem).value)


  //Insertion domain methods:

  /**
    * @return true if the element can be inserted after specified predecessor
    */
  def isInsertableAfter(elem: Int, pred: Int): Boolean = {
    isPossibleOrRequired(elem) && insertions(elem).hasValue(pred)
  }

  /**
    * @return true if the element can currently be inserted after specified predecessor
    */
  def isCurrentlyInsertableAfter(elem: Int, pred: Int): Boolean = {
    isInsertableAfter(elem, pred) && isMember(pred)
  }

  /**
    * @return all the elements after which the element can be inserted in the domain
    */
  def allPossibleInsertionsFor(elem: Int): Iterable[Int] = {
    if(!isPossibleOrRequired(elem)) return Set()
    insertions(elem)
  }

  /**
   * @return the number of elements after which the element can be inserted in the sequence
   */
  def nPossibleInsertionsFor(elem: Int): Int = insertions(elem).size

  private def resetCurrentInsertions(elem: Int): Unit = {
    currentInsertions(elem).clear()
    if(allPossibleInsertionsFor(elem).size <= length){
      currentInsertions(elem) ++= allPossibleInsertionsFor(elem).filter(isMember)
    } else {
      if(isInsertableAfter(elem, -1)) currentInsertions(elem) += -1
      currentInsertions(elem) ++= allMembersIterator.filter(isInsertableAfter(elem, _))
    }
    insertionsUpToDate(elem) = true
  }

  /**
    * @return all the elements after which the element can be inserted in the sequence
    */
  def allCurrentInsertionsFor(elem: Int): Iterable[Int] = {
    if(!insertionsUpToDate(elem)) resetCurrentInsertions(elem)
    currentInsertions(elem)
  }

  /**
    * @return the number of elements after which the element can be inserted in the sequence
    */
  def nCurrentInsertionsFor(elem: Int): Int = {
    if(!insertionsUpToDate(elem)) resetCurrentInsertions(elem)
    currentInsertions(elem).size
  }

  /**
    * @return all the insertable elements with their possible insertion predecessors
    */
  def currentInsertionsPerElem: Map[Int, Iterable[Int]] = {
    allPossibleOrRequired.map(elem => elem -> allCurrentInsertionsFor(elem)).toMap
  }

  /**
    * @return all the current possible insertions
    */
  def allCurrentInsertions: Iterable[(Int, Int)] = {
    allPossibleOrRequired.flatMap(elem => allCurrentInsertionsFor(elem).map((elem, _)))
  }

  /**
    * Excludes the possible predecessor prev from the possible insertions for elem.
    */
  def removeInsertion(elem: Int, pred: Int): Unit = {
    if(insertions(elem).hasValue(pred)){
      insertions(elem).removeValue(pred)
      currentInsertions(elem) -= pred
      if(insertions(elem).isEmpty || nCurrentInsertionsFor(elem) == 0) excludes(elem)
    }
  }

  def removeInsertionsAfter(pred: Int): Unit = {
    for(elem <- allPossibleOrRequired) removeInsertion(elem, pred)
  }

  def clearInsertionsFor(elem: Int): Unit = {
    insertions(elem).makeEmpty()
  }


  //Propagation methods:

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

  def changed(seqVar: DeltaSeqVar): Boolean = membersChanged(seqVar) || setDomain.changed(seqVar.asInstanceOf[DeltaSetBasedSeqVar].deltaSet)

  def deltaMembers(seqVar: DeltaSeqVar): Iterator[Int] = seqVar.deltaRequired().filter(isMember)

  def filterWhenDomainChangesWithDelta(idempotent: Boolean = false, priority: Int = CPStore.MaxPriorityL2 - 2)(filter: DeltaSeqVar => Unit)(implicit constraint: Constraint): DeltaSeqVar = {
    val propagator = new PropagatorSeqVar(this, 0, filter)
    propagator.idempotent = idempotent
    propagator.priorityL2 = priority
    callPropagateWhenDomainChanges(propagator)
    propagator.snapshot
  }

  override def foreach[U](f: Int => U): Unit = allMembersIterator.foreach(f)
}

object CPInsertSeqVar {
  def apply(size: Int)(implicit store: CPStore): CPInsertSeqVar = new CPInsertSeqVar(store, size)

  def apply(elemIds: Set[Int])(implicit store: CPStore): CPInsertSeqVar = {
    val size = elemIds.max
    val seqVar = new CPInsertSeqVar(store, size)
    for (elem <- 0 until size if !elemIds.contains(elem)) {
      seqVar.excludes(elem)
    }
    seqVar
  }
}



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

package oscar.cp.core

import java.util.Collection

import oscar.algo.Inconsistency
import oscar.algo.array.ArrayQueue
import oscar.algo.search.{DFSearchNode, IntConstrainableContext, SetConstrainableContext}
import oscar.cp.constraints.EqCons
import oscar.algo.vars.{IntVarLike, SetVarLike}
import oscar.cp.core.variables.{CPBoolVar, CPIntVar, CPSetVar}
import oscar.cp.core.watcher.PropagEventQueueVarSet

import scala.collection.JavaConverters._

import scala.reflect.ClassTag
import scala.util.Random

/**
 * Constraint Programming CPStore
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class CPStore(final val propagStrength: CPPropagStrength) extends DFSearchNode with IntConstrainableContext with SetConstrainableContext {

  def this() = this(CPPropagStrength.Weak)
  
  // Random object
  private[this] val rand = new Random(0)

  // Propagation queue L1 (AC5)
  private[this] val propagQueueL1 = Array.fill(CPStore.MaxPriorityL1 + 1)(new ArrayQueue[() => Unit](1000))
  private[this] var highestPriorL1 = -1

  // Propagation queue L2 (AC3)
  private[this] val propagQueueL2 = Array.fill(CPStore.MaxPriorityL2 + 1)(new ArrayQueue[Constraint](100))
  private[this] var highestPriorL2 = -1

  private[this] val cutConstraints = new ArrayQueue[Constraint](1) // usually empty

  // Total time spent in the fixed point algorithm
  private[this] var timeInFixedPoint: Long = 0

  // True if the store is executing the fixed point algorithm
  private[this] var inFixedPoint = false
  
  // Number of times an L1 filtering is called during the fix point
  private[this] var nCallsL1 = 0L  
  
  // Number of times an L1 filtering is called during the fix point
  private[this] var nCallsL2 = 0L   
  
  override def resetStats(): Unit = {
    super.resetStats()
    timeInFixedPoint = 0
    nCallsL1 = 0
    nCallsL2 = 0
  }
  
  def statistics = new SolverStatistics(nCallsL1,nCallsL2,timeInFixedPoint)
  

  // Reference to the last constraint called
  private[this] var lastConstraint: Constraint = null

  /**
   *  Returns the last constraint called in the propagate algorithm.
   *
   *  Note that `null` is returned if no constraint has been called.
   *
   *  @return The last constraint called by the propagate algorithm.
   */
  def lastConstraintCalled: Constraint = lastConstraint

  // Cleans the propagation queues
  def cleanQueues(): Unit = {
    // Clean queue L1
    while (highestPriorL1 >= 0) {
      propagQueueL1(highestPriorL1).clear()
      highestPriorL1 -= 1
    }
    // Clean queue L2
    while (highestPriorL2 >= 0) {
      val queue = propagQueueL2(highestPriorL2)
      var i = queue.size
      while (i > 0) {
        i -= 1
        val constraint = queue.removeFirst()
        constraint.setDequeued()
      }
      highestPriorL2 -= 1
    }
  }

  // Adds the constraint in the L2 queue
  @inline final def enqueueL2(c: Constraint): Unit = {
    if (c.isEnqueuable) {
      c.setEnqueued()
      val priority = c.priorityL2
      propagQueueL2(priority).addLast(c)
      if (priority > highestPriorL2) {
        highestPriorL2 = priority
      }
    }
  }

  // Adds the constraint in the L1 queue
  @inline final def enqueueL1(c: Constraint, priority: Int, evt: => Unit): Unit = {
    propagQueueL1(priority).addLast(() => {
      if (c.isActive) {
        lastConstraint = c // last constraint called
        evt
      }
    })
    if (priority > highestPriorL1) {
      highestPriorL1 = priority
    }
  }

  // set variable

  def notifyRequired(constraints: PropagEventQueueVarSet, x: CPSetVar, v: Int): Unit = {
    var q = constraints
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive) {
        enqueueL1(c, c.priorityBindL1, c.valRequired(x, v))
      }
      q = q.next
    }
  }

  def notifyRequiredIdx(constraints: PropagEventQueueVarSet, x: CPSetVar, v: Int): Unit = {
    var q = constraints
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive) {
        enqueueL1(c, c.priorityBindL1, c.valRequiredIdx(x, idx, v))
      }
      q = q.next
    }
  }

  def notifyExcluded(constraints: PropagEventQueueVarSet, x: CPSetVar, v: Int): Unit = {
    var q = constraints
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive) {
        enqueueL1(c, c.priorityBindL1, c.valExcluded(x, v))
      }
      q = q.next
    }
  }

  def notifyExcludedIdx(constraints: PropagEventQueueVarSet, x: CPSetVar, v: Int): Unit = {
    var q = constraints
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive) {
        enqueueL1(c, c.priorityBindL1, c.valExcludedIdx(x, idx, v))
      }
      q = q.next
    }
  }

  final def doAndPropagate(action: => Unit): Unit = {
    action // apply action
    if (isFailed)
      throw Inconsistency
    propagate()
  }
  
  /**
   *  Call the propagate function of the constraints and trigger the fix point algorithm
   *
   *  Note that the constraints are not added to the model
   *
   *  @param constraints a sequence of constraints
   */
  def propagate(constraints: Constraint*): Unit = {
    if (isFailed)
      throw Inconsistency

    constraints.foreach(c => enqueueL2(c))
    propagate()
  }

  protected def propagate(): Unit = {
    if (isFailed)
      throw Inconsistency
    else {
      val t = System.currentTimeMillis()
      inFixedPoint = true
      try {
        fixedPoint()
        timeInFixedPoint += System.currentTimeMillis() - t
        inFixedPoint = false
      }
      catch {
        case i: Inconsistency => {
          cleanQueues() // may be not empty
          fail()
          timeInFixedPoint += System.currentTimeMillis() - t
          inFixedPoint = false
          throw i
        }
      }
    }
  }

  @inline private def fixedPoint(): Unit = {

    // Adds the cut constraints
    cutConstraints.foreach(c => enqueueL2(c))

    while (highestPriorL1 >= 0 || highestPriorL2 >= 0) {
      // Propagate L1
      while (highestPriorL1 >= 0) {
        val queue = propagQueueL1(highestPriorL1)
        if (queue.isEmpty) highestPriorL1 -= 1
        else {
          nCallsL1 += 1
          val event = queue.removeFirst()
          event()
        }
      }

      // Propagate L2 if no constraint in L1
      while (highestPriorL1 < 0 && highestPriorL2 >= 0) {
        val queue = propagQueueL2(highestPriorL2)
        if (queue.isEmpty) highestPriorL2 -= 1
        else {
          nCallsL2 += 1
          val constraint = queue.removeFirst()
          constraint.setDequeued()
          lastConstraint = constraint
          constraint.execute()
        }
      }
    }
  }

  def printQueues(): Unit = {
    println("----------")
    propagQueueL1.foreach(q => println("L1: " + q.size))
    propagQueueL2.foreach(q => println("L2: " + q.size))
  }

  /**
   * Add a constraint to the store in a reversible way and trigger the fix-point algorithm. <br>
   * In a reversible way means that the constraint is present in the store only for descendant nodes.
   * @param constraint
   * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint).
   * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
   */
  @throws[Inconsistency]
  def post(constraint: Constraint, st: CPPropagStrength): Unit = {
    if (isFailed)
      throw Inconsistency
    try {
      constraint.setup(st)
    }
    catch {
      case e: Inconsistency =>
        fail()
        throw e
    }
    if (!inFixedPoint) { // already in a try block
      propagate()
    }
  }

  @throws[Inconsistency]
  def post(c: Constraint): Unit = post(c, propagStrength)
  
  def postCut(c: Constraint): Unit = postCut(c, propagStrength)

  def postCut(c: Constraint, st: CPPropagStrength): Unit = {
    post(c, st)
    cutConstraints.addLast(c)
  }

  def resetCuts(): Unit = {
    for (c <- cutConstraints) {
      c.deactivate() // we cannot really remove them because they were set-up
    }
    cutConstraints.clear()
  }

  /**
   * Add a constraint b == true to the store (with a Weak propagation strength) in a reversible way and trigger the fix-point algorithm. <br>
   * In a reversible way means that the constraint is present in the store only for descendant nodes.
   * @param b, the Boolean Var
   * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise
   */
  def post(b: CPBoolVar): Unit = post(new EqCons(b, 1), propagStrength)

  /**
   * Add a set of constraints to the store in a reversible way and trigger the fix-point algorithm afterwards.
   * In a reversible way means that the posted constraints are present in the store only for descendant nodes.
   * @param constraints
   * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint)
   * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
   */
  def post(constraints: Array[Constraint], st: CPPropagStrength): Unit = {
    if (isFailed)
      throw Inconsistency

    try {
      var i = 0
      while (i < constraints.length) {
        val constraint = constraints(i)
        constraint.setup(st)
        i += 1
      }
      if(!inFixedPoint)
        propagate()
    }
    catch {
      case i: Inconsistency =>
        fail()
        throw i
    }
  }

  def post(constraints: Array[Constraint]): Unit = post(constraints, propagStrength);

  /**
   * Add a set of constraints to the store (with a Weak propagation strength) in a reversible way and trigger the fix-point algorithm afterwards.
   * In a reversible way means that the posted constraints are present in the store only for descendant nodes.
   * @param constraints
   * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
   */
  def post(constraints: Collection[Constraint], st: CPPropagStrength): Unit = post(constraints.asScala.map(x => x.asInstanceOf[Constraint]).toArray, st)

  def post(constraints: Collection[Constraint]): Unit = post(constraints.asScala.toArray.map(x => x.asInstanceOf[Constraint]), propagStrength)

  @inline
  def assign(x: IntVarLike, v: Int): Unit = assign(x.asInstanceOf[CPIntVar], v)
  @inline
  def remove(x: IntVarLike, v: Int): Unit = remove(x.asInstanceOf[CPIntVar], v)
  @inline
  def smallerEq(x: IntVarLike, v: Int): Unit = smallerEq(x.asInstanceOf[CPIntVar], v)
  @inline
  def largerEq(x: IntVarLike, v: Int): Unit = largerEq(x.asInstanceOf[CPIntVar], v)
  @inline
  def remove(x: IntVarLike, vs: Array[Int]): Unit = remove(x.asInstanceOf[CPIntVar], vs)
  @inline
  def requires(x: SetVarLike, v: Int): Unit = requires(x.asInstanceOf[CPSetVar], v)
  @inline
  def excludes(x: SetVarLike, v: Int): Unit = excludes(x.asInstanceOf[CPSetVar], v)

  def assign(x: CPIntVar, v: Int): Unit = {
    if (isFailed)
      throw Inconsistency

    try {
      x.assign(v)
      propagate()
    }
    catch {
      case i: Inconsistency => {
        fail()
        throw i
      }
    }
  }


  def remove(x: CPIntVar, v: Int): Unit = {
    if (isFailed)
      throw Inconsistency

    try {
      x.removeValue(v)
      propagate()
    }
    catch {
      case i: Inconsistency => {
        fail()
        throw i
      }
    }
  }

  def remove(x: CPIntVar, vs: Array[Int]): Unit = {
    if (isFailed)
      throw Inconsistency

    try {
      x.removeValues(vs)
      propagate()
    }
    catch {
      case i: Inconsistency => {
        fail()
        throw i
      }
    }
  }

  def smallerEq(x: CPIntVar, v: Int): Unit = {
    if (isFailed)
      throw Inconsistency

    try {
      x.updateMax(v)
      propagate()
    }
    catch {
      case i: Inconsistency => {
        fail()
        throw i
      }
    }
  }

  def largerEq(x: CPIntVar, v: Int): Unit = {
    if (isFailed)
      throw Inconsistency

    try {
      x.updateMin(v)
      propagate()
    }
    catch {
      case i: Inconsistency => {
        fail()
        throw i
      }
    }
  }

  def add[T: ClassTag](boolVars: Iterable[CPBoolVar]): Unit = post(boolVars)

  def post(boolVars: Iterable[CPBoolVar]): Unit = {
    for (b <- boolVars) {
      post(b.constraintTrue)
    }
  }

  def add(c: Constraint, st: CPPropagStrength): Unit = post(c, st)

  def add(c: Constraint): Unit = add(c, propagStrength)

  def add(b: CPBoolVar): Unit = post(new EqCons(b, 1))
  
  def addCut(c: Constraint): Unit = postCut(c)

  /**
   * Add a set of constraints to the store in a reversible way and trigger the fix-point algorithm afterwards.
   * In a reversible way means that the posted constraints are present in the store only for descendant nodes.
   * @param constraints
   * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint)
   * @throws NoSolutionException if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
   */
  def add(constraints: Array[Constraint], st: CPPropagStrength): Unit = post(constraints, st)

  def add(constraints: Array[Constraint]): Unit = add(constraints, propagStrength)

  def add(constraints: Iterable[Constraint], st: CPPropagStrength): Unit = add(constraints.toArray, st)

  def add(constraints: Iterable[Constraint]): Unit = add(constraints.toArray, propagStrength)

  def +=(c: Constraint, st: CPPropagStrength): Unit = add(c, st)
  def +=(c: Constraint): Unit = add(c, propagStrength)

  /**
    * Post v \in x
    */
  def requires(x: CPSetVar, v: Int): Unit = {
    if (isFailed)
      throw Inconsistency

    try {
      x.requires(v)
      propagate()
    }
    catch {
      case i: Inconsistency => {
        fail()
        throw i
      }
    }
  }

  /**
    * Post v \notin x
    */
  def excludes(x: CPSetVar, v: Int): Unit = {
    if (isFailed)
      throw Inconsistency

    try {
      x.excludes(v)
      propagate()
    }
    catch {
      case i: Inconsistency => {
        fail()
        throw i
      }
    }
  }
}

object CPStore {

  /** The highest priority for an Level 1 filtering method */
  @deprecated("Use MaxPriorityL1 instead", "OscaR 3.1.0") val MAXPRIORL1 = 2

  /** The highest priority for the propagate method i.e. L2 */
  @deprecated("Use MaxPriorityL2 instead", "OscaR 3.1.0") val MAXPRIORL2 = 7

  /** The highest priority for an Level 1 filtering method */
  val MaxPriorityL1 = 2

  /** The lowest priority for an Level 1 filtering method */
  val MinPriorityL1 = 0

  /** The highest priority for an Level 2 filtering method */
  val MaxPriorityL2 = 7

  /** The lowest priority for an Level 2 filtering method */
  val MinPriorityL2 = 0
}

class SolverStatistics(
  val nCallsL1: Long,
  val nCallsL2: Long,
  val timeInFixPoint: Long) {
  override val toString: String = s"nCallsL1: $nCallsL1\nnCallsL2: $nCallsL2\ntimeInFixedPoint(ms): $timeInFixPoint"
}

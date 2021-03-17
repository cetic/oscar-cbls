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
package oscar.cbls.lib.constraint

import oscar.cbls.core.computation.{CBLSIntVar, ChangingIntValue, Domain, IntNotificationTarget, IntValue, Invariant, Value}
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.{SortedMap, SortedSet}

/**
 * Implement the Disjunctive constraint.
 * @param start: the start time of each task
 * @param duration: the duration of each task
 * @author Jean-Noël Monette 
 */
case class DisjunctiveConstDuration(start: Array[IntValue],
                                    duration: Array[Long]) extends Invariant with Constraint with IntNotificationTarget{

  registerStaticAndDynamicDependencyArrayIndex(start)
  registerConstrainedVariables(start)
  finishInitialization()

  private val sumdur = duration.sum

  private val Violation: CBLSIntVar = new CBLSIntVar(model, 0L, Domain(0, sumdur*start.length), "ViolationOfDisjunctive")
  Violation.setDefiningInvariant(this)

  //the degree of violation of a task is the sum of the sizes of its overlap with other tasks.
  private val Violations: SortedMap[IntValue, CBLSIntVar] = start.foldLeft(
    SortedMap.empty[IntValue, CBLSIntVar])(
    (acc, intvar) => {
      val newvar = new CBLSIntVar(model, 0L, Domain(0, sumdur), s"Violation_Disjunctive_${intvar.name}")
      acc + ((intvar, newvar))
    })

  for(i <- start.indices){
    val curstart = start(i).value
    val curduration = duration(i)
    val curend = curstart+curduration
    for(j <- i+1 until start.length){
      val nextstart = start(j).value
      val nextduration = duration(j)
      val nextend = nextstart + nextduration
      val overlap = math.max(0L,math.min(math.min(nextend-curstart,curend-nextstart),math.min(curduration,nextduration)))
      Violations(start(i)) :+= overlap
      Violations(start(j)) :+= overlap
      Violation :+= overlap
    }
  }

  //oldstarts is used to make sure that only one variable is handled at a time.
  private val oldstarts = start.map(v => v.value)

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, oldstart: Long, newstart: Long): Unit = {
    //TODO: This is not completely incremental (but still linear instead of quadratic)!
    val dur = duration(index)
    val oldend = oldstart + dur
    val newend = newstart + dur
    for(j <- start.indices){
      if(j!=index){
        val js = oldstarts(j)
        val jd = duration(j)
        val je = js + jd
        val oldoverlap = math.max(0L,math.min(math.min(je-oldstart,oldend-js),math.min(dur,jd)))
        val newoverlap = math.max(0L,math.min(math.min(je-newstart,newend-js),math.min(dur,jd)))
        Violations(v) :+= (newoverlap-oldoverlap)
        Violations(start(j)) :+= (newoverlap-oldoverlap)
        Violation :+= (newoverlap-oldoverlap)
      }
    }
    oldstarts(index) = newstart
  }

  override def violation = Violation

  override def violation(v: Value): IntValue = {
    Violations(v.asInstanceOf[IntValue])
  }

  override def checkInternals(c: Checker): Unit = {
    //TODO
  }
}

/**
 * Implement the Disjunctive constraint.
 * @param start: the start time of each task
 * @param duration: the duration of each task
 * @author Jean-Noël Monette
 * @author Renaud De Landtsheer
 */
case class Disjunctive(start: Array[IntValue],
                       duration: Array[IntValue]) extends Invariant with Constraint with IntNotificationTarget{

  require(start.length == duration.length,"start and duration array do not have the same size?!")

  val taskIndices:Range = start.indices
  val nbTask = start.length

  registerStaticAndDynamicDependencyArrayIndex(start)
  registerStaticAndDynamicDependencyArrayIndex(duration,-nbTask)

  registerConstrainedVariables(start)
  registerConstrainedVariables(duration)
  finishInitialization()

  private val sumMaxDur = duration.foldLeft(0L)((acc,v) => acc + v.max)

  private val violationVar: CBLSIntVar = new CBLSIntVar(model, 0L, Domain(0L,sumMaxDur*start.length), "ViolationOfDisjunctive")
  violationVar.setDefiningInvariant(this)

  private val violationVarsArray = Array.tabulate(start.length)(i => {
    val newVar = new CBLSIntVar(model, 0L, Domain(0L,sumMaxDur), s"Violation_Disjunctive_${start(i).name}_and_${duration(i).name}")
    newVar.setDefiningInvariant(this)
    newVar}
  )

  //the degree of violation of a task is the sum of the sizes of its overlap with other tasks.
  //opnly tasks with nonZero durations are taken into account (obviously)
  private val violationsVarsMap: SortedMap[IntValue, CBLSIntVar] = start.indices.foldLeft(
    SortedMap.empty[IntValue, CBLSIntVar])(
    (acc, i) => {
      val newVar = violationVarsArray(i)
      acc + ((start(i),newVar)) + ((duration(i),newVar))
    })

  private var nonZeroTasks:SortedSet[Int] = SortedSet.empty[Int] ++ taskIndices.filter(i => duration(i).value != 0L)

  for(taskID <- nonZeroTasks){
    val curStart = start(taskID).value
    val curDuration = duration(taskID).value
    val curEnd = curStart + curDuration

    for(otherTaskID <- nonZeroTasks if taskID < otherTaskID){
      val otherStart = start(otherTaskID).value
      val otherDuration = duration(otherTaskID).value
      val otherEnd = otherStart + otherDuration
      val overlap = math.max(0L,
        math.min(
          math.min(otherEnd-curStart,curEnd-otherStart),
          math.min(curDuration,otherDuration)))
      violationVarsArray(taskID) :+= overlap
      violationVarsArray(otherTaskID) :+= overlap
      violationVar :+= overlap
    }
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, oldValue: Long, newValue: Long): Unit = {
    if(index <0L){
      //a duration has changed
      notifyDurChanged(index + nbTask,oldValue, newValue)
    }else{
      // a start has changed
      notifyStartChanged(index,oldValue, newValue)
    }
  }

  private def notifyStartChanged(taskID:Int,oldStart:Long,newStart:Long): Unit = {
    val dur = duration(taskID).value
    if (dur == 0L) return

    val oldEnd = oldStart + dur
    val newEnd = newStart + dur

    updateTask(taskID,oldStart,newStart,oldEnd,newEnd)
  }

  private def notifyDurChanged(taskID:Int,oldDur:Long,newDur:Long): Unit ={
    if(oldDur == 0L && newDur !=0L){
      nonZeroTasks = nonZeroTasks + taskID
    }else if (oldDur !=0L && newDur == 0L){
      nonZeroTasks = nonZeroTasks - taskID
    }

    val startTask = start(taskID).value
    val oldEnd = startTask + oldDur
    val newEnd = startTask + newDur

    updateTask(taskID,startTask,startTask,oldEnd,newEnd)
  }

  def updateTask(taskID:Int,oldStart:Long,newStart:Long,oldEnd:Long,newEnd:Long): Unit ={
    //TODO: This is not completely incremental (but still linear instead of quadratic)!
    //We cannot break symmetries here because they are already broken since this method is called with one task set
    for(otherTaskID <- nonZeroTasks if taskID != otherTaskID){
      val otherStart = start(otherTaskID).value
      val otherDuration = duration(otherTaskID).value
      val otherEnd = otherStart + otherDuration
      val oldOverlap = math.max(0L,math.min(otherEnd,oldEnd)-math.max(otherStart, oldStart))
      val newOverlap = math.max(0L,math.min(otherEnd,newEnd)-math.max(otherStart, newStart))
      val deltaViolation = newOverlap - oldOverlap
      if(deltaViolation !=0L) {
        violationVarsArray(taskID) :+= deltaViolation
        violationVarsArray(otherTaskID) :+= deltaViolation
        violationVar :+= deltaViolation
      }
    }
  }

  override def violation = violationVar

  override def violation(v: Value): IntValue = {
    violationsVarsMap(v.asInstanceOf[IntValue])
  }

  override def checkInternals(c: Checker): Unit = {
    var violationFromScratch = 0L
    val violationArrayFromScratch = Array.fill(nbTask)(0L)

    for(taskID <- taskIndices){
      val curStart = start(taskID).value
      val curDuration = duration(taskID).value
      val curEnd = curStart + curDuration

      for(otherTaskID <- taskIndices if taskID < otherTaskID){
        val otherStart = start(otherTaskID).value
        val otherDuration = duration(otherTaskID).value
        val otherEnd = otherStart + otherDuration
        val overlap = math.max(0L,
          math.min(
            math.min(otherEnd-curStart,curEnd-otherStart),
            math.min(curDuration,otherDuration)))
        violationArrayFromScratch(taskID) += overlap
        violationArrayFromScratch(otherTaskID) += overlap
        violationFromScratch += overlap
      }
    }

    c.check(violation.value == violationFromScratch)
    for(t <- taskIndices){
      c.check(violation(start(t)).value == violationArrayFromScratch(t))
      c.check(violation(duration(t)).value == violationArrayFromScratch(t))
    }

    val nonZeroTasksFromScratch = SortedSet.empty[Long] ++ taskIndices.filter(i => duration(i).value != 0L)
    c.check(nonZeroTasksFromScratch equals nonZeroTasks)
  }
}

/**
  * Implement the Disjunctive constraint with margins when transitioning from one task to the other one.
  * if tasks have a duration of zero, they can overlap in time
  * we do not consider margins around tasks with zero duration
  * @param start: the start time of each task
  * @param duration: the duration of each task
  * @param marginMatrix: a matrix from task to other task to margin to have between the end of the first task and the start of the other task
  * @author Renaud De Landtsheer
  *
  */
case class DisjunctiveWithTransitions(start: Array[IntValue],
                                      duration: Array[IntValue],
                                      marginMatrix:Array[Array[Long]],
                                      ignoreZeroDurationTasks:Boolean = true) extends Invariant with Constraint with IntNotificationTarget{

  require(start.length == duration.length,"start and duration array do not have the same size?!")
  require(start.length == marginMatrix.length,"start and margin array do not have the same size?!")
  require(marginMatrix.forall((line:Array[Long]) => line.length == start.length), "not all lines of matrix have proper size")

  val taskIndices:Range = start.indices
  val nbTask = start.length

  registerStaticAndDynamicDependencyArrayIndex(start)
  registerStaticAndDynamicDependencyArrayIndex(duration,-nbTask)

  registerConstrainedVariables(start)
  registerConstrainedVariables(duration)
  finishInitialization()

  private val sumMaxDur = duration.foldLeft(0L)((acc,v) => acc + v.max)

  private val violationVar: CBLSIntVar = new CBLSIntVar(model, 0L, Domain(0, sumMaxDur*start.length), "ViolationOfDisjunctive")
  violationVar.setDefiningInvariant(this)

  private val violationVarsArray = Array.tabulate(start.length)(i => {
    val newVar = new CBLSIntVar(model, 0L, Domain(0, sumMaxDur), s"Violation_Disjunctive_${start(i).name}_and_${duration(i).name}")
    newVar.setDefiningInvariant(this)
    newVar}
  )

  //the degree of violation of a task is the sum of the sizes of its overlap with other tasks.
  //opnly tasks with nonZero durations are taken into account (obviously)
  private val violationsVarsMap: SortedMap[IntValue, CBLSIntVar] = start.indices.foldLeft(
    SortedMap.empty[IntValue, CBLSIntVar])(
    (acc, i) => {
      val newVar = violationVarsArray(i)
      acc + ((start(i),newVar)) + ((duration(i),newVar))
    })

  private var nonZeroTasks:SortedSet[Int] = SortedSet.empty[Int] ++ taskIndices.filter(i => duration(i).value != 0L)

  //margin is always put to the end of the tasks
  for(taskID <- nonZeroTasks){
    val curStart = start(taskID).value
    val curDuration = duration(taskID).value
    val curEnd = curStart + curDuration

    for(otherTaskID <- nonZeroTasks if taskID < otherTaskID){
      val otherStart = start(otherTaskID).value
      val otherDuration = duration(otherTaskID).value
      val otherEnd = otherStart + otherDuration

      val curMargin = marginMatrix(taskID)(otherTaskID)
      val otherMargin = marginMatrix(otherTaskID)(taskID)

      val overlap = math.max(0L,
        math.min(
          math.min(otherEnd + otherMargin - curStart,curEnd + curMargin - otherStart),
          math.min(curDuration + curMargin,otherDuration + otherMargin)))

      violationVarsArray(taskID) :+= overlap
      violationVarsArray(otherTaskID) :+= overlap
      violationVar :+= overlap
    }
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, oldValue: Long, newValue: Long): Unit = {
    if(index <0L){
      //a duration has changed
      notifyDurChanged(index + nbTask,oldValue, newValue)
    }else{
      // a start has changed
      notifyStartChanged(index,oldValue, newValue)
    }
  }

  private def notifyStartChanged(taskID:Int,oldStart:Long,newStart:Long): Unit = {
    val dur = duration(taskID).value
    if (dur == 0L) return

    val oldEnd = oldStart + dur
    val newEnd = newStart + dur

    updateTask(taskID,oldStart,newStart,oldEnd,newEnd)
  }

  private def notifyDurChanged(taskID:Int,oldDur:Long,newDur:Long): Unit ={
    if(oldDur == 0L && newDur !=0L){
      nonZeroTasks = nonZeroTasks + taskID
    }else if (oldDur !=0L && newDur == 0L){
      nonZeroTasks = nonZeroTasks - taskID
    }

    val startTask = start(taskID).value
    val oldEnd = startTask + oldDur
    val newEnd = startTask + newDur

    updateTask(taskID,startTask,startTask,oldEnd,newEnd)
  }

  def updateTask(taskID:Int,oldStart:Long,newStart:Long,oldEnd:Long,newEnd:Long): Unit ={
    //TODO: This is not completely incremental (but still linear instead of quadratic)!
    //We cannot break symmetries here because they are already broken since this method is called with one task set
    for(otherTaskID <- nonZeroTasks if taskID != otherTaskID){
      val otherStart = start(otherTaskID).value
      val otherDuration = duration(otherTaskID).value
      val otherEnd = otherStart + otherDuration

      val curMargin = marginMatrix(taskID)(otherTaskID)
      val otherMargin = marginMatrix(otherTaskID)(taskID)

      // We need to update the margin :
      // If the route of taskID was empty before, the oldMargin is 0L ELSE curMargin
      // If the route of taskID is empty now, the newMargin is 0L ELSE curMargin
      // Otherwhise we compare with a route that only contain the margin, which make no sense
      val oldCurMargin = if(oldEnd == oldStart) 0L else curMargin
      val newCurMargin = if(newEnd == newStart) 0L else curMargin

      val oldOverlap = math.max(0L,math.min(otherEnd + otherMargin,oldEnd + oldCurMargin)-math.max(otherStart, oldStart))
      val newOverlap = math.max(0L,math.min(otherEnd + otherMargin,newEnd + newCurMargin)-math.max(otherStart, newStart))
      val deltaViolation = newOverlap - oldOverlap

      if(deltaViolation !=0L) {
        violationVarsArray(taskID) :+= deltaViolation
        violationVarsArray(otherTaskID) :+= deltaViolation
        violationVar :+= deltaViolation
      }
    }
  }

  override def violation = violationVar

  override def violation(v: Value): IntValue = {
    violationsVarsMap(v.asInstanceOf[IntValue])
  }

  override def checkInternals(c: Checker): Unit = {
    var violationFromScratch = 0L
    val violationArrayFromScratch = Array.fill(nbTask)(0L)

    for(taskID <- taskIndices if duration(taskID).value > 0L){
      val curStart = start(taskID).value
      val curDuration = duration(taskID).value
      val curEnd = curStart + curDuration

      for(otherTaskID <- taskIndices if taskID < otherTaskID && duration(otherTaskID).value > 0L){
        val otherStart = start(otherTaskID).value
        val otherDuration = duration(otherTaskID).value
        val otherEnd = otherStart + otherDuration

        val curMargin = marginMatrix(taskID)(otherTaskID)
        val otherMargin = marginMatrix(otherTaskID)(taskID)

        val overlap = math.max(0L,
          math.min(
            math.min(otherEnd + otherMargin - curStart,curEnd + curMargin - otherStart),
            math.min(curDuration + curMargin, otherDuration + otherMargin)))
        violationArrayFromScratch(taskID) += overlap
        violationArrayFromScratch(otherTaskID) += overlap
        violationFromScratch += overlap
      }
    }

    c.check(violation.value == violationFromScratch,
      Some(s"Violation value should be $violationFromScratch, instead of ${violation.value}"))
    for(t <- taskIndices){
      c.check(violation(start(t)).value == violationArrayFromScratch(t),
        Some(s"Violation (start) for taskID $t : ${violation(start(t))} doesn't equal ${violationArrayFromScratch(t)}"))
      c.check(violation(duration(t)).value == violationArrayFromScratch(t),
        Some(s"Violation (duration) for taskID $t : ${violation(duration(t))} doesn't equal ${violationArrayFromScratch(t)}"))
    }

    val nonZeroTasksFromScratch = SortedSet.empty[Long] ++ taskIndices.filter(i => duration(i).value != 0L)
    c.check(nonZeroTasksFromScratch equals nonZeroTasks)
  }
}

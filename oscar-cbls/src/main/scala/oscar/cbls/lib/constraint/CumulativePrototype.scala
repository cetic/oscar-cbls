/**
  * *****************************************************************************
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
  * ****************************************************************************
  */
/**
  * @author Gustav Björdal
  */
package oscar.cbls.lib.constraint

import oscar.cbls.core.computation.{CBLSIntVar, ChangingIntValue, Domain, IntNotificationTarget, IntValue, Invariant, Store, Value}
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.numeric.{MinusOffsetPos, Prod2, Sum}

/**
  * Constrains the a resource usage to be lower than some limit.
  * The violation is the sum of the overflow at each time step:
  *  sum(t in 0L .. horizon)(max(0L,limit - usage[t]))
  *
  * @param start the start time of tasks
  * @param duration the duration of tasks
  * @param amount the amount that tasks use of the resource
  * @param limit the resource limit
  * @author gustav.bjordal@it.uu.se
  */
case class CumulativePrototype(start: Array[IntValue], duration: Array[IntValue], amount:Array[IntValue], limit:IntValue) extends Invariant with Constraint with IntNotificationTarget{
  for (v <- start.indices) registerStaticAndDynamicDependency(start(v), v)
  for (v <- duration.indices) registerStaticAndDynamicDependency(duration(v), v)
  for (v <- amount.indices) registerStaticAndDynamicDependency(amount(v), v)
  registerStaticAndDynamicDependency(limit)

  registerConstrainedVariable(limit)
  registerConstrainedVariables(start)
  registerConstrainedVariables(duration)
  registerConstrainedVariables(amount)

  finishInitialization()
  private val horizon = start.map(_.max).max + duration.map(_.max).max
  private val profile = new CumulativeProfile(model,start.length,horizon,amount.map(_.max).sum,limit)
  for(v <- profile.blocks){
    v.height.setDefiningInvariant(this)
    v.width.setDefiningInvariant(this)
  }
  private val Violation = Sum(profile.blocks.map(_.blockViolation))

  for(v <- start.indices){
    profile.change(start(v).value, duration(v).value, amount(v).value)
  }

  val variableViolation = start.map(i => CBLSIntVar(model, 0L, Domain(0L,amount.map(_.max).sum)))
  for(v <- variableViolation) v.setDefiningInvariant(this)
  updateVarViolation(0L,start.map(_.max).max + duration.map(_.max).max)

  override def violation = Violation
  override def violation(v: Value): IntValue = {
    if(start.indexOf(v.asInstanceOf[IntValue]) != -1L){
      variableViolation(start.indexOf(v.asInstanceOf[IntValue]))
    }else if(v == limit){
      Violation
    }else{
      0L
    }
  }

  def updateVarViolation(s:Long, d:Long): Unit = {
    //Verify that these bounds are correct
    for(i <- start.indices
        if(start(i).value >= s && start(i).value < s+d) ||
          (start(i).value < s && start(i).value+ duration(i).value-1L > s+d) ||
          (start(i).value+duration(i).value-1L >= s && start(i).value + duration(i).value-1L < s+d)){
      variableViolation(i) := profile.getViolation(start(i).value, duration(i).value)
    }
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Long, NewVal: Long): Unit = {
    if(index == -1L){
      updateVarViolation(0L,horizon)
    }else if (start(index) == v && amount(index).value >0L) {
      //start
      val d = duration(index).value
      val a = amount(index).value
      if(Math.abs(OldVal-NewVal) < d){
        if(OldVal>NewVal){
          profile.change(NewVal+d,Math.abs(OldVal-NewVal),-a)
          profile.change(NewVal,Math.abs(OldVal-NewVal),a)

          updateVarViolation(NewVal+d,Math.abs(OldVal-NewVal))
          updateVarViolation(NewVal,Math.abs(OldVal-NewVal))
        }else{
          profile.change(OldVal+d,Math.abs(OldVal-NewVal),a)
          profile.change(OldVal,Math.abs(OldVal-NewVal),-a)
          updateVarViolation(OldVal+d,Math.abs(OldVal-NewVal))
          updateVarViolation(OldVal,Math.abs(OldVal-NewVal))
        }
      }else {
        profile.change(OldVal, d, -a)
        profile.change(NewVal, d, a)
        updateVarViolation(OldVal, d)
        updateVarViolation(NewVal, d)
      }
    } else if (duration(index) == v && amount(index).value > 0L) {
      //duration
      if (OldVal > NewVal) {
        profile.change(NewVal + start(index).value, OldVal - NewVal, -amount(index).value)
        updateVarViolation(NewVal + start(index).value, OldVal - NewVal)
      } else {
        profile.change(OldVal + start(index).value, NewVal - OldVal, amount(index).value)
        updateVarViolation(OldVal + start(index).value, NewVal - OldVal)
      }
    } else if(amount(index) == v){
      profile.change(start(index).value,duration(index).value,NewVal-OldVal)
      updateVarViolation(start(index).value,duration(index).value)
    }
    //println(violation)
  }

  override def checkInternals(c: Checker): Unit = {c.check(false, Some("TODO: Implement checkinternal for CumulativeSparse"))}
}

class CumulativeProfile(m:Store, val nTasks:Int, val horizon:Long, val maxHeight:Long, var limit:IntValue){
  val blocks:Array[ProfileBlock] = Array.tabulate(2*nTasks+4)( i => new ProfileBlock(0L,0L,CBLSIntVar(m,0L,Domain(0L ,maxHeight)),limit,horizon))

  val freeBlocks = blocks(0)
  for( i <- 1 until blocks.length)
    freeBlocks.insertBlock(blocks(i))

  val endBlock = new ProfileBlock(-1L,-2L,CBLSIntVar(m,-1L,Domain(-2L,-1L),"DummyBlock"),limit,horizon)
  val profile = new ProfileBlock(-1L,-2L,CBLSIntVar(m,-1L,Domain(-2L ,-1L),"DummyBlock"),limit,horizon)
  profile.insertBlock(endBlock)
  val initialBlock = freeBlocks.popNext()
  initialBlock.setProfile(0L,horizon,0L)
  profile.insertBlock(initialBlock)

  def printProfile(): Unit = {
    println("-------------")
    var idx = 0L
    var current = profile
    while(current != endBlock){
      for(i <- current.start to current.end) {
        val p = Array.tabulate(current.height.newValueInt)(n => "#")
        println(s"$idx|$i ${current.start} to ${current.end} \t |${current.height.newValue}")
      }
      if(current.start > current.end){
        println(s"$idx|? ${current.start} to ${current.end} \t |${current.height.newValue}")
      }
      current = current.next
      idx = idx+1L
    }
  }

  def change(start:Long, duration:Long, height:Long): Unit = {
    if(duration > 0L) {
      if (freeBlocks.next == null) {
        printProfile()
      }

      var currentProfile = profile.next

      while (currentProfile != endBlock && !currentProfile.contains(start)) currentProfile = currentProfile.next

      currentProfile.changeInterval(start, start + duration - 1L, height, freeBlocks)
    }

  }

  def getViolation(start:Long, duration:Long):Long = {
    if(duration < 1L){
      return 0L
    }
    val end = start+duration-1L
    var currentProfile = profile.next
    while(currentProfile != endBlock && ! currentProfile.contains(start)) currentProfile = currentProfile.next

    var total = 0L
    var currentStart = start
    while(currentProfile.contains(currentStart) && currentStart <= end){
      val tmpEnd = Math.min(end,currentProfile.end)
      total = total + (tmpEnd-currentStart+1L)*currentProfile.getInternalOverLimit()
      currentStart = tmpEnd + 1L
      currentProfile = currentProfile.next
    }

    total
  }
}

//Note that a block where start = end -> width = 1L
class ProfileBlock(private[this] var _start:Long, private[this] var _end:Long, var height:CBLSIntVar, val limit:IntValue, horizon:Long ){
  var prev:ProfileBlock = null
  var next:ProfileBlock = null

  val width = CBLSIntVar(height.model, 0L, Domain(0L, horizon))
  val overLimit = MinusOffsetPos(height,limit,0L)
  val blockViolation = Prod2(width,overLimit)

  def start: Long = _start
  def start_=(newStart:Long): Unit = {
    _start = newStart
    width := _end-_start+1L
  }

  def end: Long = _end
  def end_=(newEnd:Long): Unit = {
    _end = newEnd
    width := _end-_start+1L
  }

  def getInternalOverLimit():Long = {
    Math.max(0L, height.newValue-limit.value)
  }
  def insertBlock(block:ProfileBlock): Unit = {
    block.next = next
    block.prev = this
    if(next != null)
      block.next.prev = block
    next = block
  }

  def removeBlock(): Unit = {
    prev.next = next
    if(next != null)
      next.prev = prev
    reset()
  }
  def popNext():ProfileBlock = {
    val tmp = next
    next.removeBlock()
    tmp
  }

  def setProfile(newStart:Long, newEnd:Long, newHeight:Long): Unit = {
    _start = newStart
    _end = newEnd
    height := newHeight
    width := _end-_start+1L
  }

  //Precondition: iStart >= start and iStart <= end
  def changeInterval(iStart:Long, iEnd:Long, iHeight:Long, freeBlocks:ProfileBlock): Unit = {
    assert(contains(iStart))

    if(iStart == start){
      if(iEnd == end){
        changeEntireBlock(iHeight,freeBlocks)
      }else if(iEnd < end){
        changeStartOfBlock(iEnd,iHeight,freeBlocks)
      }else{
        //Carry over into the next block
        val nextStart = end + 1L
        val tmp = next
        changeEntireBlock(iHeight,freeBlocks)
        tmp.changeInterval(nextStart,iEnd,iHeight,freeBlocks)
      }
    }else{
      if(iEnd == end){
        changeEndOfBlock(iStart,iHeight,freeBlocks)
      }else if(iEnd < end){
        splitBlock(iStart,iEnd,iHeight,freeBlocks)
      }else{
        //Carry over into the next block
        val nextStart = end + 1L
        val tmp = next
        changeEndOfBlock(iStart,iHeight,freeBlocks)
        //If we merged with the next block
        tmp.changeInterval(nextStart,iEnd,iHeight,freeBlocks)
      }
    }
  }

  def changeEntireBlock(iHeight:Long, freeBlocks:ProfileBlock): Unit = {
    if(prev.height.newValue == next.height.newValue && prev.height.newValue == height.newValue+iHeight){
      //Merge left and right block
      next.start = prev.start
      val tmp = prev
      tmp.removeBlock()
      freeBlocks.insertBlock(tmp)
      this.removeBlock()
      freeBlocks.insertBlock(this)
    }else if(prev.height.newValue == height.newValue+iHeight){
      //Merge with left block
      prev.end = end
      this.removeBlock()
      freeBlocks.insertBlock(this)
    }else if(next.height.newValue == height.newValue+iHeight){
      //Merge with right block
      next.start = start
      this.removeBlock()
      freeBlocks.insertBlock(this)
    }else{
      height :+= iHeight
    }
  }

  def changeStartOfBlock(iEnd:Long, iHeight:Long, freeBlocks:ProfileBlock): Unit = {
    if(prev.height.newValue == height.newValue+iHeight){
      prev.end = iEnd
      start = iEnd + 1L
    }else{
      val newBlock = freeBlocks.popNext()
      newBlock.setProfile(start,iEnd,height.newValue+iHeight)
      start = iEnd + 1L
      prev.insertBlock(newBlock)
    }
  }

  def changeEndOfBlock(iStart:Long, iHeight:Long, freeBlocks:ProfileBlock): Unit = {
    if(next.height.newValue == height.newValue+iHeight){
      next.start = iStart
      end = iStart - 1L
    }else{
      val newBlock = freeBlocks.popNext()
      newBlock.setProfile(iStart,end,height.newValue+iHeight)
      end = iStart - 1L
      insertBlock(newBlock)
    }
  }

  def splitBlock(iStart:Long, iEnd:Long, iHeight:Long, freeBlocks:ProfileBlock): Unit = {
    val middleBlock = freeBlocks.popNext()
    val endBlock = freeBlocks.popNext()
    endBlock.setProfile(iEnd+1L,end,height.newValue)
    middleBlock.setProfile(iStart,iEnd,height.newValue+iHeight)
    this.end = iStart-1L
    this.insertBlock(endBlock)
    this.insertBlock(middleBlock)
  }

  def contains(x:Long):Boolean = {start <= x && x <= end}

  def reset():Unit = {
    start = 0L
    end = 0L
    height := 0L
  }

}

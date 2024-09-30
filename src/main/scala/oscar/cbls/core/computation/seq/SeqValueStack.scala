// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.core.computation.seq

import oscar.cbls.algo.sequence.IntSequence

import scala.collection.mutable

/** Maintains a stack of value associated to Sequences of type
  * [[oscar.cbls.algo.sequence.IntSequence]].
  *
  * @tparam T
  *   Type of the saved values.
  */
class SeqValueStack[@specialized T] {

  private[this] val stackNotTop: mutable.Stack[(IntSequence, T)] = mutable.Stack.empty
  private[this] var _topSequence: IntSequence                    = _
  private[this] var _topValue: T                                 = _
  private[this] var _stackLevel: Int                             = -1

  /** Returns the value at the top checkpoint. */
  def topValue(checkpoint: IntSequence): T = {
    require(
      _topSequence sameIdentity checkpoint,
      s"topCheckpoint: ${_topSequence} has not  the sameIdentity as checkpoint: $checkpoint"
    )
    _topValue
  }

  /** Returns the top checkpoint. */
  def topSequence: IntSequence = _topSequence

  def stackLevel: Int = _stackLevel

  /** Add a new sequence at the top of the stack. */
  def push(seq: IntSequence, savedValue: T): Unit = {
    if (_stackLevel >= 0)
      stackNotTop.push((_topSequence, _topValue))
    _topSequence = seq
    _topValue = savedValue
    _stackLevel += 1
  }

  /** Empties the stack until `stackLevel` and then returns the value at the top. */
  def popUntilLevel(seq: IntSequence, stackLevel: Int): T = {
    popToLevel(stackLevel, included = false)
    topValue(seq)
  }

  /** Creates a new checkpoint at `stackLevel`. If `stackLevel` is lesser than the current stack
    * level, a pop is first performed.
    */
  def pushToLevel(seq: IntSequence, stackLevel: Int, savedValue: T): Unit = {
    require(0 <= stackLevel && stackLevel <= _stackLevel + 1, "checkpointLevel out of bounds")
    popToLevel(stackLevel, included = true)
    push(seq, savedValue)
  }

  /** Pop the top checkpoint */
  def pop(): Unit = {
    require(_stackLevel >= 0, "Try to pop an empty stack.")

    if (_stackLevel > 0) {
      val top = stackNotTop.pop()
      _topSequence = top._1
      _topValue = top._2
    } else {
      _topSequence = null
      _topValue = null.asInstanceOf[T]
    }
    _stackLevel -= 1
  }

  // Pop checkpoints until the wanted level
  private[this] def popToLevel(level: Int, included: Boolean): Unit = {
    if (included) while (_stackLevel >= level) pop()
    else while (_stackLevel > level) pop()
  }

}

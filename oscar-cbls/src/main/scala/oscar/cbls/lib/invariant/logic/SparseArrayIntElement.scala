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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Thomas Fayolle
  ******************************************************************************/
package oscar.cbls.lib.invariant.logic

import oscar.cbls.core.propagation.Checker
import oscar.cbls.core.computation.{ChangingIntValue, IntInvariant, IntNotificationTarget, IntValue, Store}

class SparseArrayIntElement(val indexAndValues : Array[(IntValue,IntValue)],
  index : IntValue,
  val defaultValue : Long = Long.MaxValue) extends IntInvariant with IntNotificationTarget{

  val nbIndex = indexAndValues.length

  var shallPropagate = false

  var currentIndexInSparseArray = -1

  this := defaultValue

  for (i <- 0 until nbIndex) {
    val v = indexAndValues(i)
    if (v._1.value == index.value) {
      currentIndexInSparseArray = i
      this := v._2.value
    }
    registerStaticAndDynamicDependency(v._1,2 * i)
    registerStaticAndDynamicDependency(v._2,2 * i + 1)
  }

  registerStaticAndDynamicDependency(index,2 * nbIndex)

  finishInitialization()

  override def notifyIntChanged(v : ChangingIntValue,id : Int,oldValue : Long,newValue : Long) : Unit = {
    if (id == 2 * nbIndex) {
      shallPropagate = true
    }
    else {
      if (id % 2 == 0) {
        if (id / 2 == currentIndexInSparseArray || indexAndValues(id/2)._1.value == index.value)
          shallPropagate = true
      } else {
        if (id / 2 == currentIndexInSparseArray) {
          this := newValue
        }
      }
    }
    scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    if (shallPropagate) {
      currentIndexInSparseArray = -1
      this:=defaultValue
      for (i <- (0 until nbIndex)) {
        val v = indexAndValues(i)
        if (v._1.value == index.value) {
          currentIndexInSparseArray = i
          this := v._2.value
        }
      }
    }
    shallPropagate = false
  }

  override def checkInternals(c : Checker): Unit = {
    val values = indexAndValues.map(_._2.value)
    val indexes = indexAndValues.map(_._1.value)
    // println("------ CHECK INTERNALS ------")

    // println("Sparse array: " + indexAndValues.mkString(";"))
    // println("Index: " + index)
    // println("Value: " + this.newValue)
    // println("Current Index In Sparse Array: " + currentIndexInSparseArray)

    val valuesOfIndex = indexAndValues.filter(_._1.value == index.value)

    // println("Values with index: " + valuesOfIndex.mkString(";"))

    if (valuesOfIndex.map(_._2.value).isEmpty)
      require(this.newValue == defaultValue,
        s"Since the index is not in the sparse table, the value shall be the default value (Currently ${this.newValue})")
    else
      require(valuesOfIndex.map(_._2.value).contains(this.newValue),
        "The value shall be one of the values associated to the index in the sparse array")

    // println("------ CHECK OK        ------")

  }

}

object SparseArrayIntElement {
  def apply (indexAndValues : Array[(IntValue,IntValue)],
    index : IntValue,
    m : Store,
    defaultValue : Long = Long.MaxValue) : SparseArrayIntElement = {

    val maxOfMax = indexAndValues.map(_._1.max).max
    indexAndValues.foreach(v => {
      require(v._1.max == maxOfMax,
        "All the index variables shall have the same maximum value (the length of the sparse table)")
      require(v._1.min == -1,
        s"The min value for the indexes of the sparse array shall be -1 (if no value is set) (Currently, for ${v._1}: ${v._1.domain.min})")
    })

    require(index.max <= maxOfMax,
      s"The maximum value of the index (${index.max}) shall be smaller than the max value of the indexes ($maxOfMax)")
    require(index.min >= -1,"The min value for the index shall be greater than 0")

    new SparseArrayIntElement(indexAndValues,index,defaultValue)
  }

}

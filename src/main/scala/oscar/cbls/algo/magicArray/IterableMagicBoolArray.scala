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

package oscar.cbls.algo.magicArray

/** Companion object of class IterableMagicBoolArray
  */

object IterableMagicBoolArray {

  /** create a Magical Array Of Boolean of given length
    * @param n
    *   the length
    * @return
    *   a Magical Array Of Boolean or null if length is less than zero
    */
  def apply(n: Int, initVal: Boolean = false): IterableMagicBoolArray = {
    require(n >= 0, "cannot create magic array of negative size")
    new IterableMagicBoolArray(n, initVal)
  }
}

/** A data structure that works like [[MagicBoolArray]] (where setting all values to true or false
  * is made in constant time) but where getting the list of indices that are true is more efficient
  *
  * @param length
  *   The length of the array
  * @param initVal
  *   The initial value in the array. Default value is false
  */

class IterableMagicBoolArray(override val length: Int, initVal: Boolean = false)
    extends MagicBoolArray(length, initVal) {

  // A list that contains an over approximation of the position that are true
  // (when approximation is active)
  private var positionsAtTrueOverApproximated: List[Int] = Nil
  // A magic array that says if a value is allready in the list of position that are true
  private val isPositionInOverApproximationQList: MagicBoolArray = MagicBoolArray(length)
  // A flag that activates the over approximation
  // over approximation is only activated when all the values are set to false
  private var overApproximationIsActive: Boolean = !initVal
  // check if at least on value has been set to false
  private var anyIndividualSetToFalse: Boolean = false

  // The indices of the array as a list
  private val indicesList: List[Int] = List.tabulate(length)(identity)

  override def all_=(value: Boolean): Unit = {
    super.all = value
    if (value) {
      positionsAtTrueOverApproximated = Nil
      isPositionInOverApproximationQList.all = false
      overApproximationIsActive = false
    } else {
      positionsAtTrueOverApproximated = Nil
      isPositionInOverApproximationQList.all = false
      overApproximationIsActive = true
    }
    // nbTrue= if(value) length else 0
  }

  override def update(id: Int, value: Boolean): Unit = {
    val oldValue = this(id)
    super.update(id, value)
    if (value) {
      if (!oldValue) {
        val alreadyIsInQList = isPositionInOverApproximationQList(id)
        isPositionInOverApproximationQList(id) = true
        if (
          overApproximationIsActive &&
          !alreadyIsInQList
        ) {
          positionsAtTrueOverApproximated = id :: positionsAtTrueOverApproximated
        }
      }
    } else if (oldValue) {
      anyIndividualSetToFalse = true
    }
  }

  override def indicesAtTrue: Iterator[Int] = {
    if (!overApproximationIsActive) {
      if (anyIndividualSetToFalse) {
        super.indicesAtTrue
      } else {
        indices.iterator
      }
    } else {
      if (anyIndividualSetToFalse) {
        positionsAtTrueOverApproximated.filter(this(_)).iterator
      } else {
        positionsAtTrueOverApproximated.iterator
      }
    }
  }

  override def indicesAtTrueAsList: List[Int] = {
    if (!overApproximationIsActive) {
      if (anyIndividualSetToFalse) {
        super.indicesAtTrueAsList
      } else {
        indicesList
      }
    } else {
      if (anyIndividualSetToFalse && positionsAtTrueOverApproximated.nonEmpty) {
        positionsAtTrueOverApproximated.filter(this(_))
      } else {
        positionsAtTrueOverApproximated
      }
    }
  }
}

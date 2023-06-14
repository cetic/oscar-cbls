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

/** An array with a stack. Adding a level to the array is made in constant time. Getting a value in
  * the array is made in <code>O(level)</code> where <code>level</code> is the current level of
  * stack.
  *
  * The array works the follwing: In each level, only the values that have changed are stored in the
  * array, and we keep a trace of the values that changed at next level. To retreive a value, we go
  * down the different level until we find a value that has been marked as changed
  *
  * @param maxLevel
  *   The max number of level
  * @param initVal
  *   The inital values of the array
  * @param size
  *   The size of the array
  */

class MagicIntArrayStacked(maxLevel: Int, initVal: Int => Long, size: Int) extends Iterable[Long] {

  // The different arrays of every level of the stack
  private[this] val levelToArray: Array[Array[Long]] = Array.tabulate(maxLevel + 1)(level =>
    if (level == 0) Array.tabulate(size)(initVal) else Array.fill(size)(0L)
  )
  // The array that track the values that changed
  private[this] val levelToIsValueChangedAtNextLevel: Array[IterableMagicBoolArray] =
    Array.tabulate(maxLevel)(level => new IterableMagicBoolArray(size, false))
  // The current level of the array
  private[this] var currentLevel: Int = 0

  /** Get the current level of the stack
    *
    * @return
    *   The current level
    */
  def level: Int = currentLevel

  /** Update the value of the array. The value is only updated for the current stack level
    *
    * @param indice
    *   The indice to update
    * @param value
    *   The new value to store
    */
  def update(indice: Int, value: Long): Unit = {
    levelToArray(currentLevel)(indice) = value
    if (currentLevel != 0) levelToIsValueChangedAtNextLevel(currentLevel - 1)(indice) = true
  }

  /** Get the value stored in the array at specific index
    *
    * @param indice
    *   the index of the element
    * @return
    *   The value
    */
  def apply(indice: Int): Long = {
    var attemptLevel = currentLevel
    while (attemptLevel > 0) {
      val levelBelow = attemptLevel - 1
      if (levelToIsValueChangedAtNextLevel(levelBelow)(indice)) {
        return levelToArray(attemptLevel)(indice)
      } else {
        attemptLevel = levelBelow
      }
    }
    levelToArray(0)(indice)
  }

  /** Adds a new level to the array
    */

  def pushLevel(): Unit = {
    require(
      currentLevel < maxLevel,
      s"MagicIntArrayStacked was declaring with max $maxLevel levels; trying to push more"
    )
    levelToIsValueChangedAtNextLevel(currentLevel).all = false
    currentLevel += 1
  }

  /** Remove a level to the array. If the parameter <code>dropChanges</code> is true, all the
    * changes of the current level are dropped. If it's false, the changes of the current level are
    * kept and they are stored in the previous level.
    *
    * @param dropChanges
    *   flag to say if the changes shall be dropped or not
    */

  def popLevel(dropChanges: Boolean): Unit = {
    require(currentLevel > 0, "trying to pop level zero")
    if (dropChanges) {
      currentLevel -= 1
    } else {
      // save changes to lower level!
      val newLevel = currentLevel - 1
      for (changedID <- levelToIsValueChangedAtNextLevel(currentLevel - 1).indicesAtTrue) {
        levelToArray(newLevel)(changedID) = levelToArray(currentLevel)(changedID)
        levelToIsValueChangedAtNextLevel(newLevel)(changedID) = false
      }
      currentLevel = newLevel
    }
  }

  /**
    * Creates a scala <code>Array</code> with the values of the current level
    *
    * @return An array with the values of the current level
    */
  def cloneTopArray: Array[Long] = {
    Array.tabulate(size)(this(_))
  }

  override def iterator: Iterator[Long] = {
    cloneTopArray.iterator
  }

  override def toString: String =
    s"MagicIntArrayStacked(size:$size level:$currentLevel [${cloneTopArray.mkString(",")}])"
}

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

package oscar.algo.reversible


/**
 * @author Pierre Schaus
 */
class ReversibleSparseSubset(store: ReversibleContext, val min: Int, val max: Int) {

  val size1 = new ReversibleInt(store, 0) // subset initially empty
  val size2 = new ReversibleInt(store, max - min + 1) // superset contains everything

  val values = Array.tabulate(size2.value)(i => i)
  val indexes = Array.tabulate(size2.value)(i => i)

  def requires(value: Int): Unit = {
    assert(checkVal(value));
    if (isRequired(value)) return ;
    if (!isPossible(value)) throw new RuntimeException(s"$value cannot be required since it is even not possible")
    exchangePositions(value, values(size1.value) + min);
    size1.incr()
    assert(size1.value <= values.length);
  }

  def possibleSize = size2.value
  def requiredSize = size1.value

  /**
   * requires all possibles
   */
  def requiresAll(): Unit = {
    size1.value = size2.value
  }

  /**
   * excludes all possible not yet required
   */
  def excludesAll(): Unit = {
    size2.value = size1.value
  }

  def excludes(value: Int): Unit = {
    assert(checkVal(value))
    if (!isPossible(value)) return // it is already not possible
    if (isRequired(value)) throw new RuntimeException(s"$value is required so it cannot be excluded")
    exchangePositions(value, values(size2.value - 1) + min);
    size2.decr()
    assert(size1.value <= values.length);
  }

  def exchangePositions(value1: Int, value2: Int): Unit = {
    assert(checkVal(value1));
    assert(checkVal(value2));
    val v1 = value1 - min;
    val v2 = value2 - min;
    val i1 = indexes(v1);
    val i2 = indexes(v2);
    values(i1) = v2;
    values(i2) = v1;
    indexes(v1) = i2;
    indexes(v2) = i1;
  }

  def requiredSet: Set[Int] = {
    (for (i <- 0 until size1.value) yield min + values(i)).toSet
  }

  def possibleSet: Set[Int] = {
    (for (i <- 0 until size2.value) yield min + values(i)).toSet
  }

  def isRequired(value: Int): Boolean = {
    if (value < min || value > max) false;
    else indexes(value - min) < size1.value;
  }

  def isPossible(value: Int): Boolean = {
    if (value < min || value > max) false;
    else indexes(value - min) < size2.value
  }

  def checkVal(value: Int) = {
    assert(value >= min);
    assert(value <= max);
    true
  }

  def deltaRequired(oldRequiredSize: Int): Iterator[Int] = {
    // go from oldSize until current required size - 1
    var ind = requiredSize-1
    new Iterator[Int] {
      def next(): Int = {
        val v = values(ind)
        ind -= 1
        v + ReversibleSparseSubset.this.min
      }
      def hasNext: Boolean = {
        ind >= oldRequiredSize && ind >= 0
      }
    }
  }
  
  def deltaPossible(oldPossibleSize: Int): Iterator[Int] = {
    // go from possibleSize+1 to oldPossibleSize
    var ind = possibleSize
    new Iterator[Int] {
      def next(): Int = {
        val v = values(ind)
        ind += 1
        v + ReversibleSparseSubset.this.min
      }
      def hasNext: Boolean = {
        ind < oldPossibleSize && ind < values.size
      }
    }
  }
  
  def possibleNotRequiredValues(): Iterator[Int] = {
    var ind = requiredSize
    new Iterator[Int] {
      def next(): Int = {
        val v = values(ind)
        ind += 1
        v + ReversibleSparseSubset.this.min
      }
      def hasNext: Boolean = {
        ind < possibleSize && ind < values.size
      }
    }
  }
  
  def requiredValues(): Iterator[Int] = {
    var ind = 0
    new Iterator[Int] {
      def next(): Int = {
        val v = values(ind)
        ind += 1
        v + ReversibleSparseSubset.this.min
      }
      def hasNext: Boolean = {
        ind < requiredSize && ind < values.size
      }
    }
  }  
  
  def arbitraryPossibleNotRequired: Int = {
    if (possibleSize == requiredSize) Int.MinValue
    else values(requiredSize) + min
  }
  
  def randomPossibleNotRequired: Int = {
    if (possibleSize == requiredSize) Int.MinValue
    else {
      val n = possibleSize - requiredSize
      val i = scala.util.Random.nextInt(n)
      values(requiredSize+n) + min
    }
  }


}

object ReversibleSparseSubset {
  def apply(store: ReversibleContext, min: Int, max: Int) = new ReversibleSparseSubset(store, min, max)
  def apply(store: ReversibleContext, possible: Set[Int]) = {
    val (min, max) = (possible.min, possible.max)
    val res = new ReversibleSparseSubset(store, min, max)
    for (i <- min to max; if !possible.contains(i)) {
      res.excludes(i)
    }
    res
  }

}

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
package oscar.cbls.core.search

/**
 * @author renaud.delandtsheer@cetic.be
 */
abstract class NeighborhoodCombinator(a: Neighborhood*) extends Neighborhood {
  //this resets the internal state of the move combinators
  override def reset(): Unit = {
    for (n <- a) n.reset()
  }

  override def resetStatistics(): Unit ={
    for (n <- a) n.resetStatistics()
  }

  override def verbose_=(i: Int): Unit = {
    for (n <- a) n.verbose = i
    super.verbose_=(i)
  }

  override def toString: String = this.getClass.getSimpleName + "(" + a.mkString(",") + ")"

  override def collectProfilingStatistics: List[Array[String]] = a.flatMap(_.collectProfilingStatistics).toList
}

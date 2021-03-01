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

package oscar.cp.core.delta

import oscar.cp.core.variables.CPIntVar

/** @author Renaud Hartert ren.hartert@gmail.com */
final class DeltaIntVarEmpty(final override val variable: CPIntVar,idx: Int = 0) extends DeltaIntVar {
  final override val id: Int = idx
  final override val oldMin: Int = variable.min
  final override val oldMax: Int = variable.max
  final override val oldSize: Int = 1
  final override val changed: Boolean = false
  final override val size: Int = 0
  final override val values: Iterator[Int] = Iterator.empty
  final override def fillArray(values: Array[Int]): Int = 0
  final override val minChanged: Boolean = false
  final override val maxChanged: Boolean = false
  final override def update(): Unit = ()
}

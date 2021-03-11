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

package oscar.cp.perf


import java.io.File

import oscar.cp.minizinc.FlatZinc2OscaR

/**
 * Minizinc competition bench 2013: instance anneal-k1
 * @author Pierre Schaus pschaus@gmail.com
 */
object PatternSetMining {
  def main(args: Array[String]): Unit = {
    val file = if ((new File("data/minizinc/pattern_set_mining.fzn")).exists()) "data/minizinc/pattern_set_mining.fzn" else  "data/minizinc/pattern_set_mining.fzn"
	val args = Array[String]("-s","-a", file)
	FlatZinc2OscaR.parse(args)
	
  }
}



package oscar.cbls.test.invariants.bench

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

import oscar.cbls.core.propagation.Checker

/**
 *
 * @param verbose indictaor to print more debugging information
 * @author yoann.guyot@cetic.be
 */
class InvariantChecker(verbose: Int = 0) extends Checker {
  var firstCheck = true
  var invariantChecked = true

  def check(verity: Boolean, traceOption: => Option[String]): Unit = {

    if (traceOption.isDefined) {
      val trace = traceOption.get
      if (!verity)
        println("Counter-example found: " + trace + " is false.")
      else if (verbose > 1) println(trace)
    }

    if (firstCheck) {
      firstCheck = false
      invariantChecked = verity
    } else {
      invariantChecked = invariantChecked && verity
    }
  }

  def isChecked: Boolean = {
    if(firstCheck) println("no check performed so far")
    invariantChecked
  }
}


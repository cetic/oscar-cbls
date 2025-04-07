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

package oscar.cbls.modeling.invariant

import oscar.cbls.core.computation.seq.SeqVariable
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.seq._
import oscar.cbls.modeling.Model

trait Seq {

  /** Returns a variable that maintains the content of the input SeqVariable.
    *
    * @param input
    *   The SeqVariable whose content is maintained.
    */
  def content(input: SeqVariable)(implicit m: Model): SetVariable = {
    val output = SetVariable(m.store, Set.empty)
    Content(m.store, input, output)
    output
  }

}

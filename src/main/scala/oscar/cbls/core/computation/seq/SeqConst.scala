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

import oscar.cbls.core.computation.Store

object SeqConst {
  private var nextNameCounter: Int = -1
  private def nextName(): String = {
    nextNameCounter += 1
    s"SeqConst_$nextNameCounter"
  }

  def apply(model: Store, initialValues: List[Int], name: String = nextName()): SeqConst ={
    new SeqConst(model, initialValues, name)
  }
}

class SeqConst(model: Store, initialValues: List[Int], name: String) extends SeqVariable(model, initialValues, name, isConstant = true )  {

}

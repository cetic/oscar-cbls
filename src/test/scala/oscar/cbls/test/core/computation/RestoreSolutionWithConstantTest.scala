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

package oscar.cbls.test.core.computation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.{IntConstant, IntVariable}
import oscar.cbls.core.computation.seq.{SeqConst, SeqVariable}
import oscar.cbls.core.computation.set.{SetConstant, SetVariable}
import oscar.cbls.lib.invariant.seq.Size
import oscar.cbls.lib.invariant.set.Union

class RestoreSolutionWithConstantTest extends AnyFunSuite with Matchers {

  test("Restore solution with SetConstant") {
    val model = new Store()
    val A     = SetConstant(model, Set.from(0 to 5))
    val B     = SetVariable(model, Set.from(2 to 7))
    val union = SetVariable(model, Set.empty)
    Union(model, A, B, union)
    model.close()
    val solution = model.extractSolution()
    B :+= 42
    model.propagate()
    noException mustBe thrownBy(solution.restoreSolution())

  }

  test("Restore solution with IntConstant") {
    val model = new Store()
    val A     = IntConstant(model, 42L)
    val B     = IntVariable(model, 0L)
    val _     = A + B
    model.close()
    val solution = model.extractSolution()
    B :+= 42
    model.propagate()
    noException mustBe thrownBy(solution.restoreSolution())
  }

  test("Restore solution with SeqConstant") {
    val model = new Store()
    val seq   = SeqConst(model, List(1, 2, 3))
    val size  = IntVariable(model, 0L)
    Size(model, seq, size)
    model.close()
    val solution = model.extractSolution()
    noException mustBe thrownBy(solution.restoreSolution())
  }

  test("Retrieving values from a Solution works as expected.") {
    val model = new Store()
    val seq   = SeqVariable(model, List(1, 2, 3))
    val size  = IntVariable(model, 0L)
    Size(model, seq, size)
    val A     = IntConstant(model, 42L)
    val B     = IntVariable(model, 0L)
    val C     = A + B
    val setA  = SetConstant(model, Set.from(0 to 5))
    val setB  = SetVariable(model, Set.from(2 to 7))
    val union = SetVariable(model, Set.empty)
    Union(model, setA, setB, union)
    model.close()

    val solution = model.extractSolution(List(C))
    B :+= 42
    setB :+= 42
    seq.insertAfterPosition(0, seq.value().explorerAtPosition(-1).get)
    C.value() should be(84)
    union.value() should be(Set(0, 1, 2, 3, 4, 5, 6, 7, 42))
    seq.value().toList should be(List(0, 1, 2, 3))

    // union was not requested, and it's not a decision variable
    solution.valueOfVariable(union) should be(None)
    // C was requested, and it's not a decision variable
    solution.valueOfVariable(C).get should be(42)
    // A and B are not requested, but they are decision variable
    solution.valueOfVariable(B).get should be(0)
    solution.valueOfVariable(A).get should be(42)

  }

}

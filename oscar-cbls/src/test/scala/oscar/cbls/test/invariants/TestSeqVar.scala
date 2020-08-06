package oscar.cbls.test.invariants

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


import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls._
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.core.propagation.ErrorChecker
import oscar.cbls.lib.invariant.seq.{Content, Length, PositionsOf}

/**
 * Created by rdl on 18-05-16.
 */
class TestSeqVar extends AnyFunSuite with Matchers {

  test("CBLSSeqVar is coherent"){
    val m = new Store(verbose = true,propagateOnToString = true, checker = Some(new ErrorChecker()))
    val a = new CBLSSeqVar(m,IntSequence(List(1,2,3,5)), n = "toto")

    val size1 = Length(a.createClone())
    val size2 = Length(a)
    val pos2 = PositionsOf(a, 2)
    val content = Content(a)

    m.registerForPartialPropagation(size2)
    m.close()

    a.insertAtPosition(45,3)
    size2.value should be(5)

    val checkpoint = a.defineCurrentValueAsCheckpoint(true)

    a.move(1,3,4,false)
    a.insertAtPosition(12,5)
    a.remove(a.value.positionOfFirstOccurrence(2).head)
    a.move(1,3,4,true)
    a.move(1,3,4,true)

    size2.value should be(5)

    a.rollbackToTopCheckpoint(checkpoint)

    size2.value should be(5)
    content.value.toList should be(List(1,2,3,5,45))
    a.value should be(checkpoint)
    size1.value should be(size2.value)
  }
}

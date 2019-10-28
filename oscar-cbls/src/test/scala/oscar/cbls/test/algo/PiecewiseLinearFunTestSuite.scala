package oscar.cbls.test.algo

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

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.algo.fun.{IdentityNaive, LinearTransform, PiecewiseLinearFun, PiecewiseLinearFunNaive}

class PiecewiseLinearFunTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {
  val maxVal = 100
  var fnFun = new PiecewiseLinearFun()
  var fnNaive:PiecewiseLinearFunNaive = IdentityNaive


  test("PiecewiseLinearFun.updateForCompositionBefore properly aligns to updateBefore on naive function"){

    update(6, 20,   new LinearTransform(26,true))
    update(7, 14,   new LinearTransform(31,true))
    update(6, 20,   new LinearTransform(-3,true))
    update(16, 19,  new LinearTransform(5,true))
    update(6, 16,   new LinearTransform(7,true))
    update(7, 14,   new LinearTransform(-3,false))
    update(6, 20,   new LinearTransform(-3,true))
    update(2, 80,   new LinearTransform(-30,true))
    update(10, 94,  new LinearTransform(10,false))
    update(3, 70,   new LinearTransform(-7,true))
    update(6, 19,   new LinearTransform(-13,true))
    update(7, 19,   new LinearTransform(2,false))
    update(7, 16,   new LinearTransform(4,false))
    update(15, 16,  new LinearTransform(-6,false))
    update(16, 16,  new LinearTransform(-11,true))
  }

  def compare(): Unit ={
    for(i <- 0 to maxVal){
      fnFun(i) should be (fnNaive(i))
    }
  }

  def update(fromIncluded:Int,toIncluded:Int,add:LinearTransform): Unit ={
    fnNaive = fnNaive.updateBefore(fromIncluded,toIncluded,add)
    fnFun = fnFun.updateForCompositionBefore(fromIncluded,toIncluded,add)
    compare()
  }
}

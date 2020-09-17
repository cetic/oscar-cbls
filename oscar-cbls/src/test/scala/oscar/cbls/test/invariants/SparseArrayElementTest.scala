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
import oscar.cbls.test.invariants.bench._
import oscar.cbls._
import oscar.cbls.lib.invariant.logic._

class SparseArrayElementTest extends AnyFunSuite {

  val verbose = 2

  val outSize = 100
  val inSize = 10
  val maxValue = 100


  test ("SparseArrayIntElement factory makes an SparseToPlentyArray") {

    val m = new Store()

    val indexAndValues : Array[(IntValue,IntValue)] = Array.tabulate(inSize)(i => (CBLSIntVar(m,0,Domain(-1,outSize),"In Index " + i),CBLSIntVar(m,0,Domain(0,maxValue),"In Value " + i)))

    val index = CBLSIntVar(m,0,Domain(0,outSize),"Index")

    assert(SparseArrayIntElement(indexAndValues,index,m).isInstanceOf[SparseArrayIntElement])
  }


  test ("SparseArrayIntElement factory detects potentially negative index") {
    val m = new Store()

    val indexAndValues : Array[(IntValue,IntValue)] = Array.tabulate(inSize)(i => (CBLSIntVar(m,0,Domain(-2,outSize),"In Index " + i),CBLSIntVar(m,0,Domain(0,maxValue),"In Value " + i)))

    val index = CBLSIntVar(m,0,Domain(0,outSize))

    assertThrows[IllegalArgumentException] {
      SparseArrayIntElement(indexAndValues,index,m)
    }

  }

  test ("SparseArrayIntElement factory detects multiple different index") {
    val m = new Store()

    val indexAndValues : Array[(IntValue,IntValue)] = Array.tabulate(inSize)(i => (CBLSIntVar(m,0,Domain(0,outSize - i),"In Index " + i),CBLSIntVar(m,0,Domain(0,maxValue),"In Value " + i)))

    val index = CBLSIntVar(m,0,Domain(0,outSize))

    assertThrows[IllegalArgumentException] {
      SparseArrayIntElement(indexAndValues,index,m)
    }

  }

  test("SparseArrayIntElement initilisation is correct") {
    val nbTest = 1000

    def indexValue = scala.util.Random.nextInt(outSize + 1) - 1

    def valueValue = scala.util.Random.nextInt(maxValue)

    for (_ <- 0 until nbTest) {
      val m = new Store()
      val indexAndValues : Array[(IntValue,IntValue)] = Array.tabulate(inSize)(i => (CBLSIntVar(m,indexValue,Domain(-1,outSize),"In index " + i),CBLSIntVar(m,valueValue,Domain(0,maxValue),"In Value " + i)))
      val index = CBLSIntVar(m,indexValue,Domain(0,outSize),"index")


      val indexAndValuesString = "---- INDEX AND VALUES ----\n" +  indexAndValues.mkString("\n") + "\n"

      val indexString = "INDEX " + index + "\n"

      val element = SparseArrayIntElement(indexAndValues,index,m)

      if (!indexAndValues.exists(_._1.value == index.value))
        assert(element.value == Long.MaxValue,indexAndValuesString + indexString + "if the index is not in the sparse array index, it shall be the default value (Currently " + element.value + ")")
      else {
        assert(indexAndValues.filter(_._1.value == index.value).map(_._2.value).contains(element.value),indexAndValuesString + indexString + "The value shall be one of the value associated to the index")

      }

    }


  }

  test ("SparseArrayIntElement is correct according to the checker") {

    val maxTest = 1000

    for (_ <- 0 until maxTest){
      val listOfPossibleMoves = List(PlusOne(),MinusOne(),ToZero(),ToMax(),ToMin(),Random(),RandomDiff(),MultipleMove(),Shuffle())

      val bench = new InvBench(verbose,listOfPossibleMoves)

      val arraySize = 100

      val indexVars = bench.genIntVars(inSize,-1 to arraySize)
      val valueVars = bench.genIntVars(inSize,0 to maxValue)
      val index = bench.genIntVar(0 to arraySize)

      val m = indexVars.head.model

      val indexAndValues : Array[(IntValue,IntValue)] = Array.tabulate(inSize)(i => (indexVars(i),valueVars(i)))

      // println(indexAndValues.mkString(";"))

      // val aMaker = SparseArrayIntElement(indexAndValues,index,m)

      // println(index)

      // println(bench.moveAndVar)


      bench.run()
    }

    // indexAndValues.foreach(iAndV =>p
    //   assert(aMaker.array(iAndV._1.value).value == iAndV._2.value))

  }

}

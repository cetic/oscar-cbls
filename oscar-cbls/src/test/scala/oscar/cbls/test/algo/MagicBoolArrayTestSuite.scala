package oscar.cbls.test.algo

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.algo.magicArray.MagicBoolArray

class MagicBoolArrayTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  test("all_(true) sets the whole array to the new value"){
    val array = MagicBoolArray(10,initVal = false)

    array.all = true
    array.indicesAtTrue.size should be (10)
  }

  test("all_(false) sets the whole array to the new value"){
    val array = MagicBoolArray(10,initVal = false)

    array.all = false
    array.indicesAtTrue.size should be (0)
  }


  val bool: Gen[Int] = for (n <- Gen.choose(0, 1)) yield n
  val boolList: Gen[List[Int]] =  for {
    numElems <- Gen.choose(20, 1000)
    elems <- Gen.listOfN(numElems, bool)
  } yield elems

  test("Updating array using .update yields the expected array"){
    forAll(boolList){list => {

      val array = MagicBoolArray(list.size, initVal = true)

      list.view.zipWithIndex.foreach{case(e,i) => array.update(i,e == 1)}

      for ((elem,i) <- list.view.zipWithIndex) {
        array(i) should be (elem == 1)
      }
    }}
  }

  test(".all_ after threshold has expected  size and content (false to true)"){

    val array = MagicBoolArray(10, initVal = false)

    array.global = Long.MaxValue-10L

    array.all = true
    array.indicesAtTrue.size should be (10)
  }

  test(".all_ after threshold has expected  size and content (true to false)"){

    val array = MagicBoolArray(10, initVal = true)

    array.global = Long.MaxValue-10L

    array.all = false
    array.indicesAtTrue.size should be (0)
  }
}

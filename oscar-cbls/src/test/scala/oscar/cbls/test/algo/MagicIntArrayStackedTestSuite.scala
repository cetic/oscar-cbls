package oscar.cbls.test.algo

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.algo.magicArray.MagicIntArrayStacked

class MagicIntArrayStackedTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {
  test("pop level below zero should fail"){
    val array = new MagicIntArrayStacked(10,int => int,10)
    array.pushLevel()
    array.pushLevel()
    array.pushLevel()

    array.popLevel(false)
    array.popLevel(false)
    array.popLevel(false)

    an [Exception] should be thrownBy array.popLevel(false)
  }

  test("push level above limit should fail"){
    val array = new MagicIntArrayStacked(2,int => int,10)

    array.pushLevel()
    array.pushLevel()

    an [Exception] should be thrownBy array.pushLevel()
  }

  test("popLevel(false) keeps the changes"){
    val array = new MagicIntArrayStacked(10,e => e,10)

    array(0) = -1
    array.pushLevel()
    array(0) = 100
    array(1) = 200
    array(2) = 300
    array.popLevel(false)

    array(0) should be (100)
  }

  test("popLevel(true) discards the changes"){
    val array = new MagicIntArrayStacked(10,e => e,10)

    array(0) = -1
    array.pushLevel()
    array(0) = 100
    array(1) = 200
    array(2) = 300
    array.popLevel(true)

    array(0) should be (-1)
  }

  test("update indice, then push then pop retrieves the expected element"){
    val array = new MagicIntArrayStacked(10,e => e,10)

    array.pushLevel()
    array(0) = 100
    array(1) = 200
    array(2) = 300
    array.pushLevel()
    array.popLevel(true)

    array(1) should be (200)
  }

  test("cloneTopArray returns expected array"){
    val array = new MagicIntArrayStacked(10,e => e,10)

    array.pushLevel()
    array(0) = 100
    array(1) = 200
    array(2) = 300
    array.pushLevel()
    array.popLevel(true)

    array.cloneTopArray should be (Array(100,200,300,3,4,5,6,7,8,9))
  }

  test("iterator yields expected array"){
    val array = new MagicIntArrayStacked(10,e => e,10)

    array.pushLevel()
    array(0) = 100
    array(1) = 200
    array(2) = 300
    array.pushLevel()
    array.popLevel(true)

    array.iterator.toList should be (List(100,200,300,3,4,5,6,7,8,9))

  }
}

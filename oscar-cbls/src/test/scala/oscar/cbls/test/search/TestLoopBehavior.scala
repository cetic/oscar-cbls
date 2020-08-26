package oscar.cbls.test.search

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import oscar.cbls.core.search.{Best, First}

class TestLoopBehavior extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with Matchers {

  test("best"){
    val a = Best()
    val (iterable,notifyFound) = a.toIterable(List(1,2,3,4,5))

    val it = iterable.toIterator

    //1
    require(it.hasNext)
    require(1 == it.next())

    require(it.hasNext)
    require(2 == it.next())

    require(it.hasNext)
    require(3 == it.next())

    notifyFound()
    require(it.hasNext)
    require(4 == it.next())

    require(it.hasNext)
    require(5 == it.next())

    require(!it.hasNext)
  }

  test("first"){
    val a = First()
    val (iterable,notifyFound) = a.toIterable(List(1,2,3,4,5))

    val it = iterable.toIterator

    //1
    require(it.hasNext)
    require(1 == it.next())

    require(it.hasNext)
    require(2 == it.next())

    require(it.hasNext)
    require(3 == it.next())

    notifyFound()
    require(!it.hasNext)
  }



  test("firstAll"){
    val a = First()
    val (iterable,notifyFound) = a.toIterable(List(1,2,3,4,5))

    val it = iterable.toIterator

    require(it.hasNext)
    require(1 == it.next())

    require(it.hasNext)
    require(2 == it.next())

    require(it.hasNext)
    require(3 == it.next())

    require(it.hasNext)
    require(4 == it.next())

    require(it.hasNext)
    require(5 == it.next())

    require(!it.hasNext)

  }



  test("first1"){
    val a = First()
    val (iterable,notifyFound) = a.toIterable(Some(1))

    val it = iterable.toIterator

    //1
    require(it.hasNext)
    require(1 == it.next())

    require(!it.hasNext)
  }

  test("best1"){
    val a = Best()
    val (iterable,notifyFound) = a.toIterable(Some(1))

    val it = iterable.toIterator

    //1
    require(it.hasNext)
    require(1 == it.next())

    require(!it.hasNext)
  }



  test("first0"){
    val a = First()
    val (iterable,notifyFound) = a.toIterable(None)

    val it = iterable.toIterator

    require(!it.hasNext)
  }

  test("best0"){
    val a = Best()
    val (iterable,notifyFound) = a.toIterable(None)

    val it = iterable.toIterator

    require(!it.hasNext)
  }
}

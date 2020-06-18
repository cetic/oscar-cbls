package oscar.cbls.test.algo

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import oscar.cbls.algo.quick.QList

import scala.util.Random

class QListTestSuite extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with Matchers {
  val elem: Gen[Long] = for (n <- Gen.choose(0, 100)) yield n * 4L // Sparse elements
  val gen: Gen[Operation] = Gen.oneOf(List(Reverse(),Insert(),Map(),Append()))

  val testBenchGen: Gen[(List[Long], List[Operation])] = for{
    numElems <- Gen.choose(20, 200)
    numActions <- Gen.choose(20, 100)
    elems <- Gen.listOfN(numElems, elem)
    actions <- Gen.listOfN(numActions, gen)
  } yield (elems,actions)

  test("Qlist keeps expected list"){
    forAll(testBenchGen,minSuccessful(200)){ testBench =>
      whenever(testBench._1.nonEmpty){
        val actionsList = testBench._2
        val referenceList = testBench._1

        var qlist = QList.buildFromIterable(referenceList)
        var modifiedList = referenceList.reverse

        for (action <- actionsList) {
          action match {
            case Reverse() =>
              qlist = qlist.reverse
              modifiedList = modifiedList.reverse

            case Insert() =>
              val newLong = Random.nextInt(10)
              qlist = QList(newLong, qlist)
              modifiedList = newLong :: modifiedList

            case Map() =>
              val toAdd = Random.nextLong()
              qlist = qlist.qMap(n => n + toAdd)
              modifiedList = modifiedList.map(n => n + toAdd)

            case Append() =>
              val toAppend = Gen.listOfN(5, Gen.choose(0L, 10)).sample.get
              qlist = QList.append(toAppend, qlist)
              modifiedList = toAppend.reverse ::: modifiedList
          }
        }

        qlist.size should be(modifiedList.size)
        qlist.iterator.toList should be(modifiedList.iterator.toList)
      }
    }
  }

  abstract sealed class Operation()
  case class Reverse() extends Operation
  case class Insert() extends Operation
  case class Map() extends Operation
  case class Append() extends Operation
}

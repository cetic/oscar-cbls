package oscar.cbls.test.algo

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer, MovedIntSequence, RemovedIntSequence}
import oscar.cbls.test.algo.SequenceTestUtils._

import scala.util.Random

class SequenceExplorerTestSuite extends FunSuite with  GeneratorDrivenPropertyChecks with Matchers{

  test("ConcreteIntSequenceExplorer is coherent"){
    forAll((referenceList: List[Long]) => {
      whenever(referenceList.size > 5){
        val seq = IntSequence(referenceList)

        seq.zipWithIndex.foreach{case (e,i) =>

          ExplorerTestUtils.compareAllAttributes(seq.explorerAtPosition(i),i,referenceList)
          ExplorerTestUtils.compareAllAttributes(seq.explorerAtLastOccurrence(e),referenceList.lastIndexOf(e),referenceList)
          ExplorerTestUtils.compareAllAttributes(seq.explorerAtFirstOccurrence(e),referenceList.indexOf(e),referenceList)
        }
      }
    })
  }

  test("MovedIntSequenceExplorer is coherent"){
    forAll((referenceList: List[Long]) => {
      whenever(referenceList.size > 5){
        val (indexFrom, indexTo, destination) = getRandomParametersForMoveAfter(referenceList)
        val seq = new MovedIntSequence(IntSequence(referenceList),indexFrom,indexTo,destination,true)
        val modifiedList = flipListManually(referenceList,indexFrom,indexTo,destination)

        seq.zipWithIndex.foreach{case (e,i) =>

          ExplorerTestUtils.compareAllAttributes(seq.explorerAtPosition(i),i,modifiedList)
          ExplorerTestUtils.compareAllAttributes(seq.explorerAtLastOccurrence(e),modifiedList.lastIndexOf(e),modifiedList)
          ExplorerTestUtils.compareAllAttributes(seq.explorerAtFirstOccurrence(e),modifiedList.indexOf(e),modifiedList)
        }
      }
    })
  }

  test("RemovedIntSequenceExplorer is coherent"){
    forAll((referenceList: List[Long]) => {
      whenever(referenceList.size > 5){

        val i = Random.nextInt(referenceList.size)
        var seq :IntSequence = new RemovedIntSequence(IntSequence(referenceList),i)
        var modifiedList = referenceList.take(i) ++ referenceList.drop(i+1)

        seq.zipWithIndex.foreach{case (e,i) =>

          ExplorerTestUtils.compareAllAttributes(seq.explorerAtPosition(i),i,modifiedList)
          ExplorerTestUtils.compareAllAttributes(seq.explorerAtLastOccurrence(e),modifiedList.lastIndexOf(e),modifiedList)
          ExplorerTestUtils.compareAllAttributes(seq.explorerAtFirstOccurrence(e),modifiedList.indexOf(e),modifiedList)
        }
      }
    })
  }
}

object ExplorerTestUtils{
  private val myTestUtils = new ExplorerTestUtils()
  def compareAllAttributes(exp :Option[IntSequenceExplorer], pos: Int,list: List[Long]): Unit = myTestUtils.compareExplorer(exp,pos,list)
}

class ExplorerTestUtils extends FunSuite with Matchers {

  /**
    * Exhaustively checks the consistency of an explorer with its reference sequence
    * @param exp The explorer to check
    * @param pos The position of the explorer in the list
    * @param list The original list
    */
  def compareExplorer(exp :Option[IntSequenceExplorer], pos: Int,list: List[Long]): Unit ={

    var explorer = exp

    list.take(pos+1).reverse.foreach(e => {
      e should be (explorer.get.value)
      explorer = explorer.get.prev
    })

    // Reached the first entry
    explorer should be (None)

    explorer = exp
    list.takeRight(list.size - pos).foreach(e => {
      e should be (explorer.get.value)
      explorer = explorer.get.next
    })

    // Reached the last entry
    explorer should be (None)

    exp.get.position should be (pos)
  }
}
package oscar.cbls.test.core.computation.seq

import org.scalatest.funsuite.AnyFunSuite
import oscar.cbls.algo.sequence.Token
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.seq.SeqVariable

import scala.util.Random

class SeqVariableSuite extends AnyFunSuite {

  // bugged seed : 1712667393905L

  private val seed: Long     = 1719327908028L//System.currentTimeMillis()
  private val random: Random = new Random(seed)

  private def isEmpty(refList: List[List[Int]]): Boolean = {
    refList.isEmpty || refList.head.isEmpty
  }

  test(
    s"Given an expected suite of operations, SeqVariable behave accordingly, using Random seed : $seed"
  ) {

    val model = new Store(debugLevel = 3)
    println(s"Run's seed : $seed")

    // The reference list, each stack represent a level of checkpoint
    // List(checkpoint2, checkpoint1, checkpoint0 (or no checkpoint))
    // checkpoint2 contains the latest state since defineCheckpoint 2
    var refListsStack: List[List[Int]] = List(List.empty)
    val seqVariable: SeqVariable       = new SeqVariable(model, List.empty, "TestSequence")
    val cloneVariable                  = seqVariable.createClone()
    model.close()

    // Two main states : 0 defined check point or at least one
    var nbOfDefinedCheckpoints: Int = 0
    var lastOperationId: Int = -1
    var lastOperationSkipped: Boolean = false
    var updateSinceLastChkptOrNoCheckpt: Boolean = true
    val seqOperations: SeqOperations = SeqOperations(random, seqVariable, cloneVariable)
    val totalNbOfOperations: Int = 1000
    for (t <- 0 until totalNbOfOperations) {
      lastOperationSkipped = false
      val operationId = random.nextInt(10)
      println(s"OpId : $operationId.\tDefined checkpoint (before) : $nbOfDefinedCheckpoints")
        operationId match {
          case 0 if updateSinceLastChkptOrNoCheckpt =>
            refListsStack = seqOperations.seqDefineCheckPointOperation(refListsStack)
            nbOfDefinedCheckpoints += 1
            updateSinceLastChkptOrNoCheckpt = false
          case 1 if seqOperations.freeValues.nonEmpty =>
            refListsStack = seqOperations.seqInsertOperation(refListsStack)
            updateSinceLastChkptOrNoCheckpt = true
          case 2 if !isEmpty(refListsStack) =>
            refListsStack = seqOperations.seqMoveOperation(refListsStack)
            updateSinceLastChkptOrNoCheckpt = true
          case 3 if !isEmpty(refListsStack) =>
            refListsStack = seqOperations.seqFlipOperation(refListsStack)
            updateSinceLastChkptOrNoCheckpt = true
          case 4 if !isEmpty(refListsStack) && refListsStack.head.size >= 2 =>
            refListsStack = seqOperations.seqSwapSegmentsOperation(refListsStack)
            updateSinceLastChkptOrNoCheckpt = true
          case 5 if !isEmpty(refListsStack)  =>
            refListsStack = seqOperations.seqRemoveOperation(refListsStack)
            updateSinceLastChkptOrNoCheckpt = true
          case 6 if nbOfDefinedCheckpoints != 0 =>
            refListsStack = seqOperations.seqRollBackToCheckPointOperation(refListsStack)
            updateSinceLastChkptOrNoCheckpt = false
          case 7 if nbOfDefinedCheckpoints != 0 =>
            refListsStack = seqOperations.seqRollBackToCheckPointOperation(refListsStack)
            refListsStack = seqOperations.seqReleaseTopCheckpoint(refListsStack)
            nbOfDefinedCheckpoints -= 1
            updateSinceLastChkptOrNoCheckpt = true
          case 8 if nbOfDefinedCheckpoints == 0 => refListsStack = seqOperations.seqAssignOperation(refListsStack)
            updateSinceLastChkptOrNoCheckpt = true
          case 9 =>
            refListsStack = seqOperations.seqPerformPropagation(refListsStack)
          case _ =>
            lastOperationSkipped = true
        }
      if(!lastOperationSkipped) {
        lastOperationId = operationId
        println(seqVariable)
      }
      println()
    }
    require(cloneVariable.value == seqVariable.value)
    println(cloneVariable.value)
  }

}


class Taken()

object Taken{
  def apply():Taken = new Taken()
}

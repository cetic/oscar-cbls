package oscar.cbls.test.core.computation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.{be, convertToAnyMustWrapper}
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.seq._
import oscar.cbls.core.computation.set.SetVariable

import scala.util.Random

class GlobalCheckpointTests extends AnyFunSuite {

  private var initInt = 0L
  private var initSet = Set(0, 1, 2, 3, 4, 5)
  private var initSeq = List(0, 1, 2, 3, 4, 5)

  test("Global checkpoint works as expected when setting it right after variables initiation.") {
    val store  = new Store(debugLevel = 3)
    val intVar = IntVariable(store, initInt)
    val setVar = SetVariable(store, initSet)
    val seqVar = SeqVariable(store, List(0, 1, 2, 3, 4, 5))

    store.close()

    val checkpoint = store.createCheckpoint()

    // Testing simple rollback on check
    doChanges(intVar, setVar, seqVar)
    checkVars(intVar, setVar, seqVar, mustBe = false)
    checkpoint.restoreCheckpoints()
    checkVars(intVar, setVar, seqVar, mustBe = true)

    // Testing rollback and release
    doChanges(intVar, setVar, seqVar)
    checkVars(intVar, setVar, seqVar, mustBe = false)
    checkpoint.restoreAndReleaseCheckpoints()
    checkVars(intVar, setVar, seqVar, mustBe = true)
    seqVar.topCheckpointLevel must be(-1)
  }

  test(
    "Global checkpoint works as expected when setting it after some modification and variables checkpoints."
  ) {
    val store  = new Store(debugLevel = 3)
    val intVar = IntVariable(store, initInt)
    val setVar = SetVariable(store, initSet)
    val seqVar = SeqVariable(store, List(0, 1, 2, 3, 4, 5))

    store.close()

    // Some moves before checkpoint
    doChanges(intVar, setVar, seqVar)

    // Saving current values for later checks
    val checkpointLevel = seqVar.topCheckpointLevel
    val checkpoint      = store.createCheckpoint()
    initInt = intVar.value()
    initSet = setVar.value()
    initSeq = seqVar.value().toList

    // Testing simple rollback on check
    doChanges(intVar, setVar, seqVar)
    checkVars(intVar, setVar, seqVar, mustBe = false)
    checkpoint.restoreCheckpoints()
    checkVars(intVar, setVar, seqVar, mustBe = true)

    // Testing rollback and release
    doChanges(intVar, setVar, seqVar)
    checkVars(intVar, setVar, seqVar, mustBe = false)
    checkpoint.restoreAndReleaseCheckpoints()
    checkVars(intVar, setVar, seqVar, mustBe = true)
    seqVar.topCheckpointLevel must be(checkpointLevel)
  }

  private def checkVars(
    intVar: IntVariable,
    setVar: SetVariable,
    seqVar: SeqVariable,
    mustBe: Boolean
  ): Unit = {
    (intVar.value() equals initInt) must be(mustBe)
    (setVar.value() equals initSet) must be(mustBe)
    (seqVar.value().toList equals initSeq) must be(mustBe)
  }

  private def doChanges(intVar: IntVariable, setVar: SetVariable, seqVar: SeqVariable): Unit = {
    for (_ <- 0 until Random.between(1, 5)) {
      seqVar.defineCurrentValueAsCheckpoint()
      seqVar.insertAfterPosition(
        Random.nextInt(100),
        seqVar.value().explorerAtPosition(seqVar.pendingValue.size - 1).get
      )
      seqVar.defineCurrentValueAsCheckpoint()
      seqVar.insertAfterPosition(
        Random.nextInt(100),
        seqVar.value().explorerAtPosition(seqVar.pendingValue.size - 1).get
      )
      intVar := Random.nextInt(100)
      setVar := Set.fill(5)(Random.nextInt(100))
    }
  }
}

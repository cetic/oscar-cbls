package oscar.cbls.test.core.computation.seq

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.{Invariant, Store}
import oscar.cbls.core.computation.seq._

/** Test Invariant used to check if the howToRollBack information are correct.
  *
  * In the [[oscar.cbls.core.computation.seq.SeqIdentityInvariant]] we do not use the howToRollBack
  * information. We have no use of it. But in some Invariant implementation this information is
  * crucial.
  *
  * To test it, this Invariant maintains a copy of the input SeqVariable using an internal
  * List[Int]. This list is updated using the howToRollBack information. After each propagation the
  * output value is set using the := method. The [[oscar.cbls.algo.sequence.IntSequence]]'s
  * [[oscar.cbls.algo.sequence.Token]] of the input and the output won't be the same but their
  * values should.
  *
  * @param store
  *   The model to which the input and output are linked
  * @param input
  *   The input SeqVariable
  * @param output
  *   The output SeqVariable
  */
private class TestSeqInvariant(store: Store, input: SeqVariable, output: SeqVariable)
    extends Invariant(store)
    with SeqNotificationTarget {

  registerStaticallyListenedElement(input)
  input.registerDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  private var listCopy: List[Int] = {
    output := input.value()
    input.value().toList
  }

  override def notifySeqChanges(
    v: SeqVariable,
    contextualVarIndex: Int,
    changes: SeqUpdate
  ): Unit = {
    digestChanges(changes)
    output := IntSequence(listCopy)
  }

  private def digestChanges(changes: SeqUpdate): Unit = {
    changes match {
      case SeqUpdateInsert(
            value: Int,
            insertAfterPositionExplorer: IntSequenceExplorer,
            prev: SeqUpdate
          ) =>
        digestChanges(prev)
        val afterPos = insertAfterPositionExplorer.position
        listCopy = listCopy
          .slice(0, afterPos + 1) ::: List(value) ::: listCopy.slice(afterPos + 1, listCopy.length)
      case SeqUpdateMove(
            fromIncludedExplorer: IntSequenceExplorer,
            toIncludedExplorer: IntSequenceExplorer,
            afterExplorer: IntSequenceExplorer,
            flip: Boolean,
            prev: SeqUpdate
          ) =>
        digestChanges(prev)
        val fromPos  = fromIncludedExplorer.position
        val toPos    = toIncludedExplorer.position
        val afterPos = afterExplorer.position
        val movedSubList: List[Int] =
          if (flip) listCopy.slice(fromPos, toPos + 1).reverse
          else listCopy.slice(fromPos, toPos + 1)
        val refListWithoutSubList: List[Int] =
          listCopy.take(fromPos) ::: listCopy.drop(toPos + 1)
        listCopy =
          if (afterPos < fromPos)
            refListWithoutSubList.take(afterPos + 1) ::: movedSubList :::
              refListWithoutSubList.drop(afterPos + 1)
          else
            refListWithoutSubList.take(afterPos + 1 - movedSubList.size) ::: movedSubList :::
              refListWithoutSubList.drop(afterPos + 1 - movedSubList.size)
      case SeqUpdateRemove(removePositionExplorer: IntSequenceExplorer, prev: SeqUpdate) =>
        digestChanges(prev)
        val pos = removePositionExplorer.position
        listCopy = listCopy.take(pos) ::: listCopy.drop(pos + 1)

      case SeqUpdateAssign(s) =>
        listCopy = s.toList
      case SeqUpdateLastNotified(value: IntSequence) =>
        assert(value.toList equals listCopy)
      case SeqUpdateRollBackToTopCheckpoint(
            _: IntSequence,
            howToRollBack: SeqUpdate,
            _: Int,
            _: SeqUpdate
          ) =>
        digestChanges(howToRollBack)
      case SeqUpdateReleaseTopCheckpoint(prev: SeqUpdate, _: IntSequence) =>
        digestChanges(prev)
      case SeqUpdateDefineCheckpoint(prev: SeqUpdate, _: Int) =>
        digestChanges(prev)
    }
  }

  /** Allows to check and debug propagation elements.
    *
    * This method can be called after the propagation according to the debug level of the
    * propagation structure (see [[oscar.cbls.core.propagation.PropagationStructure]]). It can be
    * used to check if the invariant worked properly by, for example, recomputing the value from
    * scratch.
    */
  override def checkInternals(): Unit = {
    require(
      input.value().toList == output.value().toList,
      s"Should be ${input.value().toList} got ${output.value().toList}"
    )
  }
}

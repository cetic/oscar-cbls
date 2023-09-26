package oscar.cbls.algo.sequence.stackedUpdate

import oscar.cbls.algo.sequence.IntSequenceExplorer

class RemovedIntSequenceExplorer(
  seq: RemovedIntSequence,
  val position: Int,
  explorerInOriginalSeq: IntSequenceExplorer
) extends IntSequenceExplorer {
  override val value: Int = explorerInOriginalSeq.value

  override def prev: Option[IntSequenceExplorer] = {
    explorerInOriginalSeq.prev match {
      case None => None
      case Some(tentativePos) =>
        if (tentativePos.position == seq.positionOfDelete)
          tentativePos.prev match {
            case None => None
            case Some(secondTentativePos) =>
              Some(new RemovedIntSequenceExplorer(seq, position - 1, secondTentativePos))
          }
        else Some(new RemovedIntSequenceExplorer(seq, position - 1, tentativePos))
    }
  }

  override def next: Option[IntSequenceExplorer] = {
    explorerInOriginalSeq.next match {
      case None => None
      case Some(tentativePos) =>
        if (tentativePos.position == seq.positionOfDelete)
          tentativePos.next match {
            case None => None
            case Some(secondTentativePos) =>
              Some(new RemovedIntSequenceExplorer(seq, position + 1, secondTentativePos))
          }
        else Some(new RemovedIntSequenceExplorer(seq, position + 1, tentativePos))
    }
  }
}

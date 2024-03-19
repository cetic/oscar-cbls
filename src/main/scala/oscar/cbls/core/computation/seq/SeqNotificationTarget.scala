package oscar.cbls.core.computation.seq

trait SeqNotificationTarget {
  def notifySeqChanges(v: SeqVariable, d: Int, changes: SeqUpdate): Unit
}

package oscar.cbls.core.search

trait AcceptanceCriterion {
  def apply(oldValue: Long, newValue: Long): Boolean
}

object AcceptAll extends AcceptanceCriterion {
  override def apply(oldValue: Long, newValue: Long): Boolean = true
}

object StrictImprovement extends AcceptanceCriterion {
  override def apply(oldValue: Long, newValue: Long): Boolean = newValue < oldValue
}

case class StrictlyBetterThan(value: Long) extends AcceptanceCriterion {
  override def apply(oldValue: Long, newValue: Long): Boolean = newValue < value
}

case class DifferentOf(value: Long) extends AcceptanceCriterion {
  override def apply(oldValue: Long, newValue: Long): Boolean = newValue != value
}

case class OverrideCriterion(overridingCriterion: AcceptanceCriterion) extends AcceptanceCriterion {
  override def apply(oldValue: Long, newValue: Long): Boolean = {
    (oldValue == Long.MaxValue || newValue != Long.MaxValue) && overridingCriterion(oldValue, newValue)
  }
}

package oscar.cbls.algo.sequence

/** Abstract class used to explore an [[IntSequence]]
 *
 * Each instance represents the state of the sequence at a specific position.
 */
abstract class IntSequenceExplorer {
  // The value at the current position
  val value: Int
  // Returns the position of the value
  def position: Int
  // Returns the next explorer in the sequence or None if end of sequence
  def next: Option[IntSequenceExplorer]
  // Returns the previous explorer in the sequence or None if start of sequence
  def prev: Option[IntSequenceExplorer]
}

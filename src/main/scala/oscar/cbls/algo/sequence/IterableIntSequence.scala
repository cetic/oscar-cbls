package oscar.cbls.algo.sequence

/** [[IntSequence]] as an [[Iterable]] of [[Int]]
  *
  * @param sequence
  *   The IntSequence to convert
  */
class IterableIntSequence(sequence: IntSequence) extends Iterable[Int] {
  override def iterator: Iterator[Int] = sequence.iterator

  override def head: Int = sequence.valueAtPosition(0).head

  override def headOption: Option[Int] = sequence.valueAtPosition(0)

  override def last: Int = sequence.valueAtPosition(sequence.size - 1).head

  override def lastOption: Option[Int] = sequence.valueAtPosition(sequence.size - 1)
}

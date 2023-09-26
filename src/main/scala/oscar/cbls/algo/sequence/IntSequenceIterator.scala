package oscar.cbls.algo.sequence

/** [[IntSequence]] as an [[Iterator]] of [[Int]]
  *
  * It uses the logic of [[IntSequenceExplorer]] to perform its tasks
  * @param intSequenceExplorer
  *   The optional [[IntSequenceExplorer]]
  */
class IntSequenceIterator(var intSequenceExplorer: Option[IntSequenceExplorer])
    extends Iterator[Int] {

  override def hasNext: Boolean =
    intSequenceExplorer match {
      case None    => false
      case Some(_) => true
    }

  override def next(): Int = {
    val position = intSequenceExplorer.head
    intSequenceExplorer = position.next
    position.value
  }
}

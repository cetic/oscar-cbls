package oscar.cbls.algo.sequence

import scala.language.implicitConversions

/** This class is used to generate an [[IntSequenceIterator]] from a [[IntSequenceExplorer]]
  *
  * @param intSequenceExplorer
  *   The starting point of any IntSequenceIterator created in this class
  * @param forward
  *   A flag.
  *   - true : Iterating using IntSequenceExplorer.next
  *   - false : Iterating using IntSequenceExplorer.prev
  */
case class IntSequenceExplorerToIterator(intSequenceExplorer: IntSequenceExplorer, forward: Boolean)
    extends Iterable[IntSequenceExplorer] {

  /** Returns an IntSequenceIterator iterating until an [[IntSequenceExplorer]] with the
    * corresponding value is found or no [[IntSequenceExplorer]] are remaining.
    *
    * @param value
    *   The value to match
    * @return
    *   An IntSequenceIterator, forward or backward
    */
  def untilValue(value: Int): IntSequenceIterator = until(e => e.value == value)

  /** Returns an IntSequenceIterator iterating until an [[IntSequenceExplorer]] fulfilling the
    * condition is found or no [[IntSequenceExplorer]] are remaining.
    *
    * @param f
    *   The condition to fulfill
    * @return
    *   An IntSequenceIterator, forward or backward
    */
  def until(f: IntSequenceExplorer => Boolean): IntSequenceIterator = {
    if (forward)
      IntSequenceForwardIterator(intSequenceExplorer, f, inclusive = false)
    else
      IntSequenceBackwardIterator(intSequenceExplorer, f, inclusive = false)
  }

  /** Same as untilValue but including the [[IntSequenceExplorer]] with the corresponding value */
  def toValue(value: Int): IntSequenceIterator = to(e => e.value == value)

  /** Same as untilValue but including the [[IntSequenceExplorer]] fulfilling the condition */
  def to(f: IntSequenceExplorer => Boolean): IntSequenceIterator = {
    if (forward)
      IntSequenceForwardIterator(intSequenceExplorer, f, inclusive = true)
    else
      IntSequenceBackwardIterator(intSequenceExplorer, f, inclusive = true)
  }

  /** Returns an IntSequenceIterator iterating until no IntSequenceExplorer are remaining */
  override def iterator: Iterator[IntSequenceExplorer] = {
    if (forward) IntSequenceForwardIterator(intSequenceExplorer, _ => false, inclusive = true)
    else IntSequenceBackwardIterator(intSequenceExplorer, _ => false, inclusive = true)
  }
}

abstract class IntSequenceIterator extends Iterator[IntSequenceExplorer]

/** An conditional iterator going through the sequence using next moves on explorer
  *
  * NOTE : It allows to start RootIntSequenceExplorer "before start" but not "after end"
  *
  * @param start
  *   The starting explorer of the iterator
  * @param stopCondition
  *   As long as it's false ==> continue
  * @param inclusive
  *   Whether or not we should include the last [[IntSequenceExplorer]]
  */
case class IntSequenceForwardIterator(
  var start: IntSequenceExplorer,
  stopCondition: IntSequenceExplorer => Boolean,
  inclusive: Boolean
) extends IntSequenceIterator {
  private var _next: IntSequenceExplorer = start
  private var _hasNext: Boolean =
    (start.position >= -1 && start.position < start.intSequence.size) &&
      (inclusive || start.position == -1 || !stopCondition(start))

  override def hasNext: Boolean = _hasNext

  override def next(): IntSequenceExplorer = {
    if (!_hasNext) throw new NoSuchElementException
    val value = _next
    _hasNext = {
      val currentFulFillCondition   = !(start.position == -1) && stopCondition(value)
      lazy val nextFulFillCondition = stopCondition(value.next)
      value.position < value.intSequence.size - 1 && (
        (!inclusive && !currentFulFillCondition && !nextFulFillCondition) ||
          (inclusive && !currentFulFillCondition)
      )
    }
    _next = value.next
    value
  }
}

/** An conditional iterator going through the sequence using prev moves on explorer
  *
  * NOTE : It allows to start RootIntSequenceExplorer "after end" but not "before start"
  *
  * @param start
  *   The starting explorer of the iterator
  * @param stopCondition
  *   As long as it's false ==> continue
  * @param inclusive
  *   Whether or not we should include the last [[IntSequenceExplorer]]
  */
case class IntSequenceBackwardIterator(
  var start: IntSequenceExplorer,
  stopCondition: IntSequenceExplorer => Boolean,
  inclusive: Boolean
) extends IntSequenceIterator {
  private var _next: IntSequenceExplorer = start
  private var _hasNext: Boolean =
    (start.position >= 0 && start.position <= start.intSequence.size) &&
      (inclusive || start.position == start.intSequence.size || !stopCondition(start))

  override def hasNext: Boolean = _hasNext

  override def next(): IntSequenceExplorer = {
    if (!_hasNext) throw new NoSuchElementException
    val value = _next
    _hasNext = {
      val currentFulFillCondition =
        !(start.position == start.intSequence.size) && stopCondition(value)
      lazy val nextFulFillCondition = stopCondition(value.prev)
      value.position > 0 && (
        (!inclusive && !currentFulFillCondition && !nextFulFillCondition) ||
          (inclusive && !currentFulFillCondition)
      )
    }
    _next = value.prev
    value
  }
}

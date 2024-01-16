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

case class IntSequenceForwardIterator(
  var start: IntSequenceExplorer,
  condition: IntSequenceExplorer => Boolean,
  inclusive: Boolean
) extends IntSequenceIterator {
  private var _next: IntSequenceExplorer = start
  private var _hasNext: Boolean =
    (start.position >= 0 && start.position < start.intSequence.size) &&
      (inclusive || !condition(start))

  override def hasNext: Boolean = _hasNext

  override def next(): IntSequenceExplorer = {
    if (!_hasNext) throw new NoSuchElementException
    val value = _next
    _hasNext = {
      val currentFulFillCondition   = condition(value)
      lazy val nextFulFillCondition = condition(value.next)
      value.position < value.intSequence.size - 1 && (
        (!inclusive && !currentFulFillCondition && !nextFulFillCondition) ||
          (inclusive && !currentFulFillCondition)
      )
    }
    _next = value.next
    value
  }
}

case class IntSequenceBackwardIterator(
  var start: IntSequenceExplorer,
  condition: IntSequenceExplorer => Boolean,
  inclusive: Boolean
) extends IntSequenceIterator {
  private var _next: IntSequenceExplorer = start
  private var _hasNext: Boolean =
    (start.position >= 0 && start.position < start.intSequence.size) &&
      (inclusive || !condition(start))

  override def hasNext: Boolean = _hasNext

  override def next(): IntSequenceExplorer = {
    if (!_hasNext) throw new NoSuchElementException
    val value = _next
    _hasNext = {
      val currentFulFillCondition   = condition(value)
      lazy val nextFulFillCondition = condition(value.prev)
      value.position > 0 && (
        (!inclusive && !currentFulFillCondition && !nextFulFillCondition) ||
          (inclusive && !currentFulFillCondition)
      )
    }
    _next = value.prev
    value
  }
}

package oscar.cbls.algo.sequence

/** Represents an empty [[IntSequence]].
  *
  * Only insertion is permitted. Upon insertion it creates a [[ConcreteIntSequence]] containing the
  * value.
  */
case class EmptyIntSequence() extends IntSequence(depth = 0) {
  override def size: Int = 0

  override def iterator: Iterator[Int] = Iterator.empty

  override def nbOccurrence(value: Int): Int = 0

  override def unorderedContentNoDuplicate: List[Int] = List.empty

  override def unorderedContentNoDuplicateWithNBOccurrences: List[(Int, Int)] = List.empty

  override def valueAtPosition(position: Int): Option[Int] = None

  override def positionsOfValue(value: Int): List[Int] = List.empty

  override def contains(value: Int): Boolean = false

  override def explorerAtPosition(position: Int): Option[IntSequenceExplorer] =
    if (position == -1) Some(new RootIntSequenceExplorer(this, true))
    else None

  /** Insert a value after the position defined by the [[IntSequenceExplorer]]
    *
    * In this particular case, we simply create a [[ConcreteIntSequence]] with the value as the only
    * element in the sequence. Thus the explorer and fast flag are not mandatory an have default
    * values.
    * @param value
    *   The value to insert
    * @param insertAfterPositionExplorer
    *   The position after which to insert the value
    * @param fast
    *   Fast flag (for more detail see description)
    * @return
    *   An IntSequence with the new value
    */
  override def insertAfterPosition(
    value: Int,
    insertAfterPositionExplorer: IntSequenceExplorer =
      new RootIntSequenceExplorer(this, backward = true),
    fast: Boolean = false
  ): IntSequence = {
    IntSequence(List(value))
  }

  override def remove(removePosAsExplorer: IntSequenceExplorer, fast: Boolean): IntSequence = {
    require(requirement = false, "Can't remove a value if the sequence is empty")
    this
  }

  override def moveAfter(
    fromIncludedExplorer: IntSequenceExplorer,
    toIncludedExplorer: IntSequenceExplorer,
    moveAfterExplorer: IntSequenceExplorer,
    flip: Boolean,
    fast: Boolean
  ): IntSequence = {
    require(requirement = false, "Can't move values if the sequence is empty")
    this
  }

  override def regularizeToMaxPivot(
    maxPivotPerValuePercent: Int,
    targetToken: Token
  ): EmptyIntSequence = this

  override def regularize(targetToken: Token): EmptyIntSequence = this

  override def commitPendingMoves: IntSequence = this

  override def descriptorString: String =
    "[]_impl:empty"
}

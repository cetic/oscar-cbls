package oscar.cbls.test.algo.sequence

import org.scalacheck.Gen
import oscar.cbls.algo.sequence.IntSequence

import java.util.concurrent.atomic.AtomicInteger

object IntSequenceOperationsGenerator {

  def elem: Gen[Int] = for (n <- Gen.choose(0, 100)) yield n * 4 // Sparse elements

  // Insert operation generator
  def opInsert(size: Int): Gen[Insert] = for {
    value    <- Gen.choose(0, 1000)
    position <- Gen.choose(0, size - 1)
  } yield {
    Insert(value, position)
  }

  // Move operation generator
  def opMoveAfter(size: Int): Gen[MoveAfter] =
    for {
      from  <- Gen.choose(0, size - 1)
      to    <- Gen.choose(from, size - 1)
      after <- Gen.oneOf((-1 until from).toList ::: (to until size - 1).drop(1).toList)
      flip  <- Gen.prob(0.5)
    } yield {
      MoveAfter(from, to, after, flip)
    }

  // Remove operation generator
  def opDelete(size: Int): Gen[Delete] = {
    // if the list is empty, Delete(0) will be ignored, keeping it to avoid usage of Gen.Option
    if (size <= 1) Delete(0)
    else
      for {
        position <- Gen.choose(0, size - 1)
      } yield {
        Delete(position)
      }
  }

  def opFlip: Gen[Flip]             = Gen.const(Flip())
  def opRegularize: Gen[Regularize] = Gen.const(Regularize())
  def opCommit: Gen[Commit]         = Gen.const(Commit())

  def genAction(
    onlyInsert: Boolean = false,
    onlyMove: Boolean = false,
    onlyRemove: Boolean = false,
    genSeqSize: AtomicInteger
  ): Gen[Operation] = {
    for {
      // Seems mandatory to keep it, otherwise it fails constantly
      _ <- Gen.const(genSeqSize.get())
      action <- {
        val size = genSeqSize.get()
        if (onlyInsert) opInsert(size)
        else if (onlyMove) Gen.oneOf(opMoveAfter(size), opFlip)
        else if (onlyRemove) opDelete(size)
        else
          Gen.oneOf(
            opInsert(size),
            opMoveAfter(size),
            opDelete(size),
            opFlip,
            opRegularize,
            opCommit
          )
      }
    } yield {
      action match {
        case _: Insert => genSeqSize.getAndIncrement()
        case _: Delete => genSeqSize.decrementAndGet()
        case _         =>
      }
      action
    }
  }

  def testBenchGen(
    maxConsideredSize: Int,
    genSeqSize: AtomicInteger,
    onlyInsert: Boolean = false,
    onlyMove: Boolean = false,
    onlyRemove: Boolean = false
  ): Gen[(List[Int], List[Operation])] = for {
    numElems   <- Gen.choose(20, maxConsideredSize)
    numActions <- Gen.choose(20, maxConsideredSize)
    elems <- {
      genSeqSize.set(numElems)
      Gen.listOfN(numElems, elem)
    }
    actions <- Gen.listOfN(numActions, genAction(onlyInsert, onlyMove, onlyRemove,genSeqSize))
  } yield (elems, actions)
}

abstract sealed class Operation {
  def perform(
    initialSeq: IntSequence,
    initialRefList: List[Int],
    fast: Boolean = true
  ): (IntSequence, List[Int])
}
case class MoveAfter(fromIncl: Int, toIncl: Int, after: Int, flip: Boolean) extends Operation {

  override def perform(
    initialSeq: IntSequence,
    initialRefList: List[Int],
    fast: Boolean = true
  ): (IntSequence, List[Int]) = {
    (
      initialSeq.moveAfter(
        initialSeq.explorerAtPosition(fromIncl).get,
        initialSeq.explorerAtPosition(toIncl).get,
        initialSeq.explorerAtPosition(after).get,
        flip = flip,
        fast = fast
      ),
      moveListManually(initialRefList)
    )
  }

  /** Implements manually the moveAfter transformation (to compare with IntSequence)
   * @param list
   *   The original list to swap
   * @return
   *   A new sequence with the proper transformation
   */
  def moveListManually(list: List[Int]): List[Int] = {

    var resultList = List[Int]()
    val moved        = {
      if(flip) list.slice(fromIncl, toIncl + 1).reverse
      else list.slice(fromIncl, toIncl + 1)
    }
    val start       = list.take(fromIncl)
    val end = if (toIncl < list.size - 1) list.takeRight(list.size - toIncl - 1) else List()

    if (after == -1) {
      resultList = moved ::: start ::: end
    } else {
      if (after < fromIncl) {
        // Insert the flip at the left
        val part1 = start.take(after + 1)
        val part2 = start.takeRight(fromIncl - after - 1)
        resultList = part1 ::: moved ::: part2 ::: end
      } else {
        // Insert the flip at the right
        val part1 = end.take(after - toIncl)
        val part2 = end.takeRight(list.size - after - 1)
        resultList = start ::: part1 ::: moved ::: part2
      }
    }
    resultList
  }
}
case class Insert(value: Int, afterPosition: Int) extends Operation {
  override def perform(
    initialSeq: IntSequence,
    initialRefList: List[Int],
    fast: Boolean = true
  ): (IntSequence, List[Int]) = {
    val newSeq =
      initialSeq.insertAfterPosition(value, initialSeq.explorerAtPosition(afterPosition).get, fast = fast)
    val (front, back) = initialRefList.splitAt(afterPosition + 1)
    val newRefList    = front ++ List(value) ++ back
    (newSeq, newRefList)
  }
}
case class Delete(pos: Int) extends Operation {
  override def perform(
    initialSeq: IntSequence,
    initialRefList: List[Int],
    fast: Boolean = true
  ): (IntSequence, List[Int]) = {
    if (initialRefList.nonEmpty)
      (
        initialSeq.delete(initialSeq.explorerAtPosition(pos).get, fast = fast),
        initialRefList.take(pos) ++ initialRefList.drop(pos + 1)
      )
    else (initialSeq, initialRefList)
  }
}
case class Flip() extends Operation {
  override def perform(
    initialSeq: IntSequence,
    initialRefList: List[Int],
    fast: Boolean = true
  ): (IntSequence, List[Int]) = {
    (initialSeq.flip(fast = fast), initialRefList.reverse)
  }
}
case class Regularize() extends Operation {
  override def perform(
    initialSeq: IntSequence,
    initialRefList: List[Int],
    fast: Boolean = true
  ): (IntSequence, List[Int]) = {
    (initialSeq.regularizeToMaxPivot(4), initialRefList)
  }
}
case class Commit() extends Operation {
  override def perform(
    initialSeq: IntSequence,
    initialRefList: List[Int],
    fast: Boolean = true
  ): (IntSequence, List[Int]) = {
    (initialSeq.commitPendingMoves, initialRefList)
  }
}

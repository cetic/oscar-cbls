// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.test.algo.sequence

import org.scalacheck._
import org.scalacheck.commands.Commands
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.util.invBench.FailureReporter

import scala.util.Try

/** Creates a test bench for [[oscar.cbls.algo.sequence.IntSequence]].
  *
  * @param maxSize
  *   The maximum size of the sequence
  * @param additionalSeeds
  *   A list of explicit seeds that will be tested in addition to a random seed. Use this when you
  *   find a bug on a specific example.
  */
private[sequence] class IntSeqTestBench(maxSize: Int, additionalSeeds: List[String] = List())
    extends Commands {

  override type State = List[Int]
  override type Sut   = IntSeqSut // Must be mutable

  override def canCreateNewSut(
    newState: State,
    initSuts: Iterable[State],
    runningSuts: Iterable[Sut]
  ): Boolean = true

  override def newSut(state: State): Sut = new IntSeqSut(state)

  override def destroySut(sut: Sut): Unit = ()

  override def initialPreCondition(state: State): Boolean = true

  override def genInitialState: Gen[State] = {
    for {
      size <- Gen.choose(1, maxSize)
      list <- Gen.sequence[List[Int], Int](List.tabulate(size)(_ => Gen.choose(-1000, 1000)))
    } yield list

  }

  override def genCommand(state: State): Gen[Command] = {
    var commands: List[(Int, Gen[Command])] =
      List((5, genInsert(state)), (1, Regularize()), (1, Commit()), (2, Check()))

    if (state.nonEmpty) {
      commands = (5, genDelete(state)) :: commands
      commands = (5, genMove(state)) :: commands
      commands = (2, genFlip()) :: commands
    }

    Gen.frequency(commands: _*)
  }

  /** Used to start the tests. */
  def test(): Unit = {
    val prop = new IntSeqBenchProperty(this)
    additionalSeeds.distinct.foreach(prop.addSeed)
    prop.check()
    if (prop.failureReporter.failedReports.nonEmpty) {
      val s = prop.failureReporter.failedReports.reverse.mkString("", "\n\n\n", "\n")
      throw new TestFailedException(s)
    }
  }

  private def genInsert(state: State): Gen[Insert] = {
    for {
      value <- Gen.choose(-1000, 1000)
      pos   <- if (state.nonEmpty) Gen.choose(-1, state.length - 1) else Gen.const(-1)
      fast  <- Gen.prob(0.1)
    } yield Insert(value, pos, fast)
  }

  private def genDelete(state: State): Gen[Delete] = {
    for {
      pos  <- Gen.choose(0, state.length - 1)
      fast <- Gen.prob(0.1)
    } yield Delete(pos, fast)
  }

  private def genMove(state: State): Gen[MoveAfter] = {
    val size: Int = state.length
    for {
      from  <- Gen.choose(0, size - 1)
      to    <- Gen.choose(from, size - 1)
      after <- Gen.oneOf((-1 until from).toList ::: (to + 1 until size - 1).toList)
      flip  <- Gen.prob(0.5)
      fast  <- Gen.prob(0.1)
    } yield MoveAfter(from, to, after, flip, fast)
  }

  private def genFlip(): Gen[Flip] = for (fast <- Gen.prob(0.1)) yield Flip(fast)

  /** Used to config the test bench */
  private class IntSeqBenchProperty(bench: IntSeqTestBench)
      extends Properties("IntSequence test bench") {
    propertyWithSeed("Random seed", None) = bench.property()

    val failureReporter: FailureReporter = FailureReporter()

    def addSeed(s: String): Unit = this.propertyWithSeed(s"$s", Some(s)) = bench.property()

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
      p.withMinSuccessfulTests(500).withTestCallback(failureReporter)
    }
  }

  /** Dedicated exception for test failure */
  private class TestFailedException(msg: String) extends Exception(msg)

  /** Command used to check if the SUT is coherent with the state */
  private case class Check() extends Command {
    override type Result = IntSequence

    override def postCondition(state: State, result: Try[Result]): Prop = {
      if (result.isFailure) Prop.falsified
      else {
        val newSeq = result.get
        val next   = nextState(state)
        try {
          SequenceTester.testIntSequence(newSeq, next)
          ExplorerTester.testExplorer(newSeq, next)
          Prop.proved
        } catch {
          case _: Exception => Prop.falsified
        }
      }
    }

    override def run(sut: Sut): Result = sut.intSeq

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true
  }

  /** Command that inserts a new value in the sequence. */
  private case class Insert(value: Int, afterPosition: Int, fast: Boolean) extends UnitCommand {

    override def postCondition(state: List[Int], success: Boolean): Prop = success

    override def run(sut: Sut): Unit = {
      val exp = sut.intSeq.explorerAtPosition(afterPosition).get
      sut.insertAfterPosition(value, exp, fast)
    }

    override def nextState(state: State): State = {
      val (front, back) = state.splitAt(afterPosition + 1)
      front ++ (value :: back)
    }

    override def preCondition(state: State): Boolean = afterPosition < state.length
  }

  /** Command that removes a value from the sequence. */
  private case class Delete(pos: Int, fast: Boolean) extends UnitCommand {

    override def postCondition(state: List[Int], success: Boolean): Prop = success

    override def run(sut: Sut): Unit = {
      val exp = sut.intSeq.explorerAtPosition(pos).get
      sut.remove(exp, fast)
    }

    override def nextState(state: State): State = state.take(pos) ++ state.drop(pos + 1)

    override def preCondition(state: State): Boolean = pos < state.length
  }

  /** Command that flips the sequence. */
  private case class Flip(fast: Boolean) extends UnitCommand {

    override def postCondition(state: List[Int], success: Boolean): Prop = success

    override def run(sut: Sut): Unit = {
      sut.flip(fast)
    }

    override def nextState(state: State): State = state.reverse

    override def preCondition(state: State): Boolean = true
  }

  /** Command that moves a sub-sequence and optionaly flips it. */
  private case class MoveAfter(from: Int, to: Int, after: Int, flip: Boolean, fast: Boolean)
      extends UnitCommand {

    override def postCondition(state: List[Int], success: Boolean): Prop = success

    override def run(sut: Sut): Unit = {
      val fromExp  = sut.intSeq.explorerAtPosition(from).get
      val toExp    = sut.intSeq.explorerAtPosition(to).get
      val afterExp = sut.intSeq.explorerAtPosition(after).get
      sut.moveAfter(fromExp, toExp, afterExp, flip, fast)
    }

    override def nextState(state: State): State = {
      var next = List[Int]()
      val moved = {
        if (flip) state.slice(from, to + 1).reverse
        else state.slice(from, to + 1)
      }
      val start = state.take(from)
      val end   = if (to < state.size - 1) state.takeRight(state.size - to - 1) else List()

      if (after == -1) {
        next = moved ::: start ::: end
      } else {
        if (after < from) {
          // Insert the flip at the left
          val part1 = start.take(after + 1)
          val part2 = start.takeRight(from - after - 1)
          next = part1 ::: moved ::: part2 ::: end
        } else {
          // Insert the flip at the right
          val part1 = end.take(after - to)
          val part2 = end.takeRight(state.size - after - 1)
          next = start ::: part1 ::: moved ::: part2
        }
      }
      next
    }

    override def preCondition(state: State): Boolean =
      from < state.length && to < state.length && after < state.length
  }

  /** Command that regularizes the pivot of maximum 5%. */
  private case class Regularize() extends UnitCommand {

    override def postCondition(state: List[Int], success: Boolean): Prop = success

    override def run(sut: Sut): Unit = sut.regularize()

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true
  }

  /** Command that commits the pending moves. */
  private case class Commit() extends UnitCommand {

    override def postCondition(state: List[Int], success: Boolean): Prop = success

    override def run(sut: Sut): Unit = sut.commitPendingMoves()

    override def nextState(state: State): State = state

    override def preCondition(state: State): Boolean = true
  }

}

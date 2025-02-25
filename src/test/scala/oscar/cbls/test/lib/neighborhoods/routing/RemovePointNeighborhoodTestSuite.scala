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

package oscar.cbls.test.lib.neighborhoods.routing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.Minimize
import oscar.cbls.core.computation.seq._
import oscar.cbls.core.computation.set.{SetConstant, SetVariable}
import oscar.cbls.core.computation.{Invariant, Store}
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.invariant.set.Diff
import oscar.cbls.lib.neighborhoods.routing.RemovePointNeighborhood
import oscar.cbls.modeling.routing.VRS
import oscar.cbls.test.lib.neighborhoods.ToolsForTestingNeighborhood.generateRandomValidRoute

import scala.annotation.tailrec
import scala.util.Random

class RemovePointNeighborhoodTestSuite extends AnyFunSuite with Matchers {

  private class SumSeqInv(
    model: Store,
    input: SeqVariable,
    output: IntVariable,
    sumCriterion: Int => Boolean = _ => true // Used to select which nodes have to be summed
  ) extends Invariant(model, None)
      with SeqNotificationTarget {

    input.registerStaticallyAndDynamicallyListeningElement(this)
    output.setDefiningInvariant(this)

    output := sumFromScratch(input.value())

    override def notifySeqChanges(
      seqVariable: SeqVariable,
      contextualVarIndex: Int,
      changes: SeqUpdate
    ): Unit = digestUpdate(changes)

    override def checkInternals(): Unit = {
      val expected = sumFromScratch(input.pendingValue)
      require(
        output.pendingValue == expected,
        s"""checkInternals fails
           |output: $output
           |expected: $expected
           |""".stripMargin
      )
    }

    private[this] def digestUpdate(changes: SeqUpdate): Unit = {
      changes match {
        case SeqUpdateAssign(newSeq: IntSequence) => output := sumFromScratch(newSeq)
        case sui: SeqUpdateInsert =>
          digestUpdate(sui.prev)
          if (sumCriterion(sui.value)) output :+= sui.value.toLong
        case sur: SeqUpdateRemove =>
          digestUpdate(sur.prev)
          if (sumCriterion(sur.explorerAtRemovePosition.value))
            output :-= sur.explorerAtRemovePosition.value
        case surbttch: SeqUpdateRollBackToTopCheckpoint =>
          digestUpdate(surbttch.howToRollBack)
        case x: SeqUpdateWithPrev =>
          digestUpdate(x.prev)
        case _: SeqUpdateLastNotified =>
        case x: SeqUpdate =>
          require(requirement = false, s"Try unhandled update $x")
      }
    }

    private[this] def sumFromScratch(seq: IntSequence): Long = {
      var toReturn = 0L
      for (x <- seq) if (sumCriterion(x)) toReturn += x.toLong
      toReturn
    }
  }

  test("RemovePoint works as expected on a simple example") {
    val model  = new Store()
    val vrs    = VRS(model, 1000, 100, debug = true)
    val output = IntVariable(model, 0L)
    val obj    = Minimize(output)
    val _      = new SumSeqInv(model, vrs.routes, output)
    model.close()

    val seed = Random.nextLong()
    val rng  = new Random(seed)

    vrs.routes := generateRandomValidRoute(vrs.n, vrs.v, rng)
    model.propagate()

    val relevantRemove: () => Iterable[Int] = () => vrs.routedWithoutVehicles.pendingValue

    val search = RemovePointNeighborhood(vrs, relevantRemove)

    withClue(s"Working with seed $seed\n") {
      noException mustBe thrownBy(search.doAllMoves(obj))
      obj.objValue.value() must be((1 until vrs.v).sum)
      vrs.routes.value().toList must contain theSameElementsAs (0 until vrs.v)
    }
  }

  ignore("RemovePoint verbose mode") {
    val model  = new Store()
    val vrs    = VRS(model, 20, 5, debug = true)
    val output = IntVariable(model, 0L)
    val obj    = Minimize(output)
    // By summing only even number we can see the effect of hot restart
    val _            = new SumSeqInv(model, vrs.routes, output)
    val canBeRemoved = SetVariable(model, Set.empty)
    val _ =
      Diff(model, vrs.routedWithVehicles, SetConstant(model, Set.from(0 until vrs.v)), canBeRemoved)
    model.close()

    val seed = Random.nextLong()
    val rng  = new Random(seed)

    vrs.routes := generateRandomValidRoute(vrs.n, vrs.v, rng)
    model.propagate()

    val relevantRemove: () => Iterable[Int] = () => canBeRemoved.pendingValue

    val search = RemovePointNeighborhood(
      vrs,
      relevantRemove,
      selectNodesToRemoveBehavior = LoopBehavior.first(),
      hotRestart = false
    )
    search.verbosityLevel = 4

    println(s"Routes before search $vrs")
    withClue(s"Working with seed $seed\n") {
      noException mustBe thrownBy(search.doAllMoves(obj))
    }
    search.displayProfiling()
    println(s"Routes after search $vrs")
  }

  ignore("RemovePoint with hot restart") {
    val model  = new Store()
    val vrs    = VRS(model, 20, 5, debug = true)
    val output = IntVariable(model, 0L)
    val obj    = Minimize(output)
    // By summing only even number we can see the effect of hot restart
    val _            = new SumSeqInv(model, vrs.routes, output, _ % 2 == 0)
    val canBeRemoved = SetVariable(model, Set.empty)
    val _ =
      Diff(model, vrs.routedWithVehicles, SetConstant(model, Set.from(0 until vrs.v)), canBeRemoved)
    model.close()

    val seed = Random.nextLong()
    val rng  = new Random(seed)

    vrs.routes := generateRandomValidRoute(vrs.n, vrs.v, rng)
    model.propagate()

    val relevantRemove: () => Iterable[Int] = () => canBeRemoved.pendingValue

    val search = RemovePointNeighborhood(
      vrs,
      relevantRemove,
      selectNodesToRemoveBehavior = LoopBehavior.first()
    )
    search.verbosityLevel = 4

    println(s"Routes before search $vrs")
    withClue(s"Working with seed $seed\n") {
      noException mustBe thrownBy(search.doAllMoves(obj))
    }
    search.displayProfiling()
    println(s"Routes after search $vrs")
  }

  ignore("RemovePoint with symmetry class") {
    val model        = new Store()
    val vrs          = VRS(model, 20, 5, debug = true)
    val output       = IntVariable(model, 0L)
    val obj          = Minimize(output)
    val _            = new SumSeqInv(model, vrs.routes, output)
    val canBeRemoved = SetVariable(model, Set.empty)
    val _ =
      Diff(model, vrs.routedWithVehicles, SetConstant(model, Set.from(0 until vrs.v)), canBeRemoved)
    model.close()

    val seed = Random.nextLong()
    val rng  = new Random(seed)

    vrs.routes := generateRandomValidRoute(vrs.n, vrs.v, rng)
    model.propagate()

    val relevantRemove: () => Iterable[Int] = () => canBeRemoved.pendingValue

    val search = RemovePointNeighborhood(
      vrs,
      relevantRemove,
      selectNodesToRemoveBehavior = LoopBehavior.best(),
      nodesToRemoveSymmetryClass = Some(x => x % 2)
    )
    search.verbosityLevel = 4

    println(s"Routes before search $vrs")
    withClue(s"Working with seed $seed\n") {
      noException mustBe thrownBy(search.doAllMoves(obj))
    }
    search.displayProfiling()
    println(s"Routes after search $vrs")
  }

}

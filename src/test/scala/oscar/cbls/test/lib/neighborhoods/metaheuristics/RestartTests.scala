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

package oscar.cbls.test.lib.neighborhoods.metaheuristics

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.{Exploration, Minimize}
import oscar.cbls.core.computation.seq.SeqVariable
import oscar.cbls.core.search.{Move, MoveFound, NoMoveFound, SimpleNeighborhood}
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.neighborhoods.metaheuristics.Restart
import oscar.cbls.lib.neighborhoods.random.RandomizeRoutesNeighborhood
import oscar.cbls.lib.neighborhoods.routing.TwoOpt
import oscar.cbls.modeling.routing.VRS
import oscar.cbls.test.lib.neighborhoods.routing.NaiveSumDistancesInvariant

import scala.util.Random

class RestartTests extends AnyFunSuite with Matchers {

  private class SwapNodeMove(
    seq: SeqVariable,
    first: IntSequenceExplorer,
    second: IntSequenceExplorer,
    obj: Long
  ) extends Move(obj, "Swap nodes") {

    override def commit(): Unit = {
      seq.swapSegments(
        first,
        first,
        flipFirstSegment = false,
        second,
        second,
        flipSecondSegment = false
      )
    }

    override def toString: String =
      s"$neighborhoodName" + super.toString + s"\nSwaps nodes ${first.value} and ${second.value}"
  }

  private class SwapNodesNeighborhood(vrs: VRS)
      extends SimpleNeighborhood[SwapNodeMove]("SwapNodes") {

    override protected def exploreNeighborhood(exploration: Exploration[SwapNodeMove]): Unit = {
      val seqVal = vrs.routes.defineCurrentValueAsCheckpoint()

      val first  = seqVal.explorerAtAnyOccurrence(0).get.next
      val second = seqVal.explorerAtAnyOccurrence(1).get.next

      vrs.routes.swapSegments(
        first,
        first,
        flipFirstSegment = false,
        second,
        second,
        flipSecondSegment = false
      )
      exploration.checkNeighborWP(objValue => new SwapNodeMove(vrs.routes, first, second, objValue))
      vrs.routes.rollbackToTopCheckpoint(Some(seqVal))
    }

    override def doMove(move: SwapNodeMove): Unit = move.commit()

    override def reset(): Unit = {}
  }

  private def generateManhattanDistMatrix(points: Array[(Int, Int)]): Array[Array[Long]] = {
    def dist(x1: (Int, Int), x2: (Int, Int)): Long = (x1._1 - x2._1).abs + (x1._2 - x2._2).abs

    Array.tabulate(points.length, points.length)((i: Int, j: Int) => dist(points(i), points(j)))
  }

  private val points: Array[(Int, Int)] =
    Array(
      (0, 0),
      (0, -10),
      (-2, 4),
      (-2, 6),
      (2, 6),
      (2, 4),
      (2, -14),
      (2, -16),
      (-2, -16),
      (-2, -14)
    )

  private val distMatrix: Array[Array[Long]] = generateManhattanDistMatrix(points)

  test("Restart works as Expected") {
    val model  = new Store()
    val vrs    = VRS(model, 10, 2, debug = true)
    val output = IntVariable(model, 0L, name = Some("Minimize"))
    new NaiveSumDistancesInvariant(model, vrs.routes, distMatrix, output)
    val obj = Minimize(output)
    model.close()
    vrs.routes := IntSequence(List(0, 4, 3, 5, 2, 1, 9, 6, 8, 7))
    model.propagate()

    val routedNodeExceptVehicle = vrs.routedWithoutVehicles.pendingValue

    val relevantStartSegment = () => routedNodeExceptVehicle

    val twoOpt =
      TwoOpt(vrs, relevantStartSegment, selectFlippedSegmentBehavior = LoopBehavior.first())
    val swap = new SwapNodesNeighborhood(vrs)

    val search = Restart(twoOpt, swap, 2, 4)

    // Four 2-opt has to be performed
    for (_ <- 0 until 4) {
      search.getMove(obj) match {
        case mf: MoveFound =>
          search.commitMove(obj, mf.move)
        case _ =>
      }
    }

    var routes = vrs.mapVehicleToRoute
    routes(0) must contain inOrderOnly (0, 2, 3, 4, 5)
    routes(1) must contain inOrderOnly (1, 9, 8, 7, 6)
    obj.objValue.value() must be(44)

    // One swap has to be performed
    search.getMove(obj) match {
      case mf: MoveFound =>
        search.commitMove(obj, mf.move)
      case _ =>
    }

    routes = vrs.mapVehicleToRoute
    routes(0) must contain inOrderOnly (0, 9, 3, 4, 5)
    routes(1) must contain inOrderOnly (1, 2, 8, 7, 6)
    obj.objValue.value() must be(100)

    // Two 2-opt has to be performed
    for (_ <- 0 until 2) {
      search.getMove(obj) match {
        case mf: MoveFound =>
          search.commitMove(obj, mf.move)
        case _ =>
      }
    }

    routes = vrs.mapVehicleToRoute
    routes(0) must contain inOrderOnly (0, 5, 4, 3, 9)
    routes(1) must contain inOrderOnly (1, 6, 7, 8, 2)
    obj.objValue.value() must be(70)

    // One swap has to be performed
    search.getMove(obj) match {
      case mf: MoveFound =>
        search.commitMove(obj, mf.move)
      case _ =>
    }

    routes = vrs.mapVehicleToRoute
    routes(0) must contain inOrderOnly (0, 6, 4, 3, 9)
    routes(1) must contain inOrderOnly (1, 5, 7, 8, 2)
    obj.objValue.value() must be(126)

    // Two 2-opt has to be performed
    for (_ <- 0 until 2) {
      search.getMove(obj) match {
        case mf: MoveFound =>
          search.commitMove(obj, mf.move)
        case _ =>
      }
    }

    routes = vrs.mapVehicleToRoute
    routes(0) must contain inOrderOnly (0, 3, 4, 6, 9)
    routes(1) must contain inOrderOnly (1, 8, 7, 5, 2)
    obj.objValue.value() must be(78)

    // No move found. The best solution must be restored
    search.getMove(obj) must be(NoMoveFound)
    routes = vrs.mapVehicleToRoute
    routes(0) must contain inOrderOnly (0, 2, 3, 4, 5)
    routes(1) must contain inOrderOnly (1, 9, 8, 7, 6)
    obj.objValue.value() must be(44)
  }

  ignore("Restart verbose mode") {
    val model  = new Store()
    val vrs    = VRS(model, 10, 2, debug = true)
    val output = IntVariable(model, 0L, name = Some("Minimize"))
    new NaiveSumDistancesInvariant(model, vrs.routes, distMatrix, output)
    val obj = Minimize(output)
    model.close()
    vrs.routes := IntSequence(List(0, 4, 3, 5, 2, 1, 9, 6, 8, 7))
    model.propagate()

    val routedNodeExceptVehicle = vrs.routedWithoutVehicles.pendingValue

    val relevantStartSegment = () => routedNodeExceptVehicle

    val twoOpt =
      TwoOpt(vrs, relevantStartSegment, selectFlippedSegmentBehavior = LoopBehavior.first())
    val swap = new SwapNodesNeighborhood(vrs)

    val search = Restart(twoOpt, swap, 2, 4)
    search.verbosityLevel = 2
    search.profileSearch()
    search.doAllMoves(obj)
    search.displayProfiling()
  }

  ignore("Search with random restart") {
    val seed   = Random.nextLong()
    val rng    = new Random(seed)
    val model  = new Store()
    val vrs    = VRS(model, 10, 2, debug = true)
    val output = IntVariable(model, 0L, name = Some("Minimize"))
    new NaiveSumDistancesInvariant(model, vrs.routes, distMatrix, output)
    val obj = Minimize(output)
    model.close()
    vrs.routes := vrs.generateValidRandomRoute(rng = rng)
    model.propagate()

    println(s"Before search $vrs")

    val relevantStartSegment = () => vrs.routedWithoutVehicles.pendingValue

    val twoOpt =
      TwoOpt(vrs, relevantStartSegment, selectFlippedSegmentBehavior = LoopBehavior.first())
    val restartNeigh = RandomizeRoutesNeighborhood.shuffle(vrs, 3, rng)
    val search       = Restart(twoOpt, restartNeigh, 4, 5)
    search.profileSearch()
    search.verbosityLevel = 2

    withClue(s"Working with seed $seed") {
      noException mustBe thrownBy(search.doAllMoves(obj))
    }
    search.displayProfiling()

    println(s"After search $vrs")
    println(s"obj: ${obj.objValue}")
  }

}

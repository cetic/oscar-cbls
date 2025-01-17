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

package oscar.cbls.test.modeling.routing

import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.seq.SeqVariable
import oscar.cbls.modeling.routing.{CachedVehicleSearcher, StackedVehicleSearcher}
import oscar.cbls.util.Tabulator

/** Object to compare the performance of a [[StackedVehicleSearcher]] and a cached vehicle searcher.
  */
object VehicleSearcherBenchmark {

  /** Task to benchmark:
    *
    * For each position in `[0, numSearch[` searches which vehicle reaches this position using the
    * given vehicle searcher.
    */
  private def searcherTask(searcher: Int => Int, numSearch: Int): Unit = {
    for (pos <- 0 until numSearch) searcher(pos)
  }

  /** @param task
    *   The task to test in the benchmark.
    * @param warmup
    *   How many times the task has to be performed to warm up the JVM.
    * @param repetition
    *   How many times repeat the task.
    * @param searchesPerformedInTask
    *   How many searches are performed during the task. Used to compute the average times to
    *   perform a search.
    * @return
    *   The total times elapsed for doing all the task and the average time to perform a search
    *   considering all the repetition performed.
    */
  private def benchmarkVehicleSearcher(
    task: => Unit,
    warmup: Int,
    repetition: Int,
    searchesPerformedInTask: Long
  ): (Long, Long) = {
    for (_ <- 0 until warmup) task

    val before: Long = System.nanoTime()
    for (_ <- 0 until repetition) task
    val after: Long = System.nanoTime()

    val totalTime: Long     = after - before
    val avgSearchTime: Long = totalTime / (repetition.toLong * searchesPerformedInTask)

    (totalTime, avgSearchTime)
  }

  def main(args: Array[String]): Unit = {
    val v                = 100   // Number of vehicle
    val n                = 20000 // Number of nodes (including depots)
    var nodes: List[Int] = List.empty

    for (vehicle <- 0 until v) {
      nodes = nodes ::: List.from(vehicle until n by v)
    }

    val model  = new Store()
    val routes = SeqVariable(model, nodes)
    val inv    = new UpdateVehicleSearchersInvariant(model, routes, v)
    model.close()
    val checkpoint = routes.defineCurrentValueAsCheckpoint()
    model.propagate()

    val warmup     = 50
    val repetition = 100
    val stackedCheckpointResults =
      benchmarkVehicleSearcher(
        searcherTask(inv.stackedVehicleSearcher.vehicleReachingPosition, n),
        warmup,
        repetition,
        n.toLong
      )

    val newCachedSearcher = CachedVehicleSearcher(checkpoint, v)

    val newCachedCheckpointResults =
      benchmarkVehicleSearcher(
        searcherTask(newCachedSearcher.vehicleReachingPosition(_, routes.pendingValue), n),
        warmup,
        repetition,
        n.toLong
      )

    for (vehicle <- 0 until v) {
      val explorer = routes.pendingValue.explorerAtAnyOccurrence(vehicle).get
      routes.insertAfterPosition(n + vehicle, explorer)
    }
    model.propagate()

    val stackedNotCheckpointResults =
      benchmarkVehicleSearcher(
        searcherTask(inv.stackedVehicleSearcher.vehicleReachingPosition, n),
        warmup,
        repetition,
        n.toLong
      )

    val newCachedNotCheckpointResults =
      benchmarkVehicleSearcher(
        searcherTask(newCachedSearcher.vehicleReachingPosition(_, routes.pendingValue), n),
        warmup,
        repetition,
        n.toLong
      )

    val formatter = java.text.NumberFormat.getIntegerInstance
    val table =
      List(
        List(
          "",
          "Avg search time at checkpoint (ns)",
          "Total time at checkpoint (ns)",
          "Avg search time not a checkpoint (ns)",
          "Total time not at checkpoint (ns)"
        ),
        List(
          "New stacked vehicle searcher",
          formatter.format(stackedCheckpointResults._2),
          formatter.format(stackedCheckpointResults._1),
          formatter.format(stackedNotCheckpointResults._2),
          formatter.format(stackedNotCheckpointResults._1)
        ),
        List(
          "New cached vehicle searcher",
          formatter.format(newCachedCheckpointResults._2),
          formatter.format(newCachedCheckpointResults._1),
          formatter.format(newCachedNotCheckpointResults._2),
          formatter.format(newCachedNotCheckpointResults._1)
        )
      )

    val title: String =
      s"""Searching vehicle for problem with $v vehicles, ${formatter.format(n)} nodes at
         |checkpoint and ${formatter.format(routes.pendingValue.size)} otherwise.
         |Performed ${formatter.format(n)} vehicle searches $repetition times.
         |""".stripMargin.replaceFirst("\n", " ")

    println(title)
    println(Tabulator.format(table))

  }
}

package examples.oscar.cbls.benchmarks

import oscar.cbls._
import oscar.cbls.algo.generator.WarehouseLocationGenerator
import oscar.cbls.core.search.First
import oscar.cbls.lib.invariant.logic.Filter
import oscar.cbls.lib.invariant.minmax.MinConstArrayLazy
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.search.neighborhoods.{AssignNeighborhood, RandomizeNeighborhood, SwapsNeighborhood}
import oscar.cbls.util.Benchmark

object WLPComparativeBench extends App {

  //the number of warehouses
  val W: Int = 1000

  //the number of delivery points
  val D: Int = 300

  println(s"ComparativeBench on WarehouseLocation(W:$W, D:$D)")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (costForOpeningWarehouse, distanceCost) = WarehouseLocationGenerator.apply(W, D, 0, 100, 3)

  val m = Store()

  val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, s"warehouse_${l}_open"))
  val openWarehouses = Filter(warehouseOpenArray).setName("openWarehouses")

  val distanceToNearestOpenWarehouse = Array.tabulate(D)(d =>
    MinConstArrayLazy(distanceCost(d).map(_.toInt), openWarehouses, defaultCostForNoOpenWarehouse).setName(s"distance_for_delivery_$d"))

  val obj = Objective(Sum(distanceToNearestOpenWarehouse) + Sum(costForOpeningWarehouse, openWarehouses))

  m.close()
  /*
    val neighborhood = (Statistics(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),true)
      exhaustBack Statistics(SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses"))
      orElse (RandomizeNeighborhood(warehouseOpenArray, W/5) maxMoves 2) saveBest obj restoreBestOnExhaust)

    neighborhood.verbose = 1

    neighborhood.doAllMoves(_>= W + D, obj)

    println(neighborhood.statistics)
  */
  val neighborhood1 = () => ("exhaustBack", AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")
    exhaustBack SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses")
    orElse (RandomizeNeighborhood(warehouseOpenArray, () => W / 5) maxMoves 2) saveBest obj restoreBestOnExhaust)

  val neighborhood2 = () => ("simulatedAnnealing", (AssignNeighborhood(
    warehouseOpenArray,
    "SwitchWarehouse",
    hotRestart = false,
    selectIndiceBehavior = First(randomized = true))
    .cauchyAnnealing(initialTemperature = 5, base = 10)
    //the two stop criterion here below can be used, although they are useless for small size example.
    //maxMoves W*50 withoutImprovementOver obj
    .cutTail(timePeriodInMilliSecond = 500, minRelativeImprovementByCut = 0.00001, minTimeBeforeFirstCutInMilliSecond = 1000)
    saveBestAndRestoreOnExhaust obj))

  val neighborhood3 = () => ("lateAcceptance", (AssignNeighborhood(
    warehouseOpenArray,
    selectIndiceBehavior = First(randomized = true),
    hotRestart = false,
    name = "SwitchWarehouse")
    .lateAcceptanceHillClimbing(20)
    saveBestAndRestoreOnExhaust obj))


  val neighborhood3b = () => ("lateAcceptanceSwichAndSwap", (AssignNeighborhood(
    warehouseOpenArray,
    selectIndiceBehavior = First(randomized = true),
    hotRestart = false,
    name = "SwitchWarehouse")
    exhaustBack SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses")
    .lateAcceptanceHillClimbing(20)
    saveBestAndRestoreOnExhaust obj))

  val neighborhood4 = () => ("best", AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")
    best SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses")
    orElse (RandomizeNeighborhood(warehouseOpenArray, () => W / 5) maxMoves 2) saveBest obj restoreBestOnExhaust)

  val neighborhoods = List(neighborhood1, neighborhood2, neighborhood3, neighborhood3b, neighborhood4)

  val a = Benchmark.benchToStringFull(obj, 3, neighborhoods, 1, verbose = 1)

  println(a)
}

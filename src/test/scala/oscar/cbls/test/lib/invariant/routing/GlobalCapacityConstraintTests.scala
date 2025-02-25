package oscar.cbls.test.lib.invariant.routing

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.{Store, Variable}
import oscar.cbls.lib.invariant.routing.capacityConstraint.GlobalCapacityConstraintWithLogReduction
import oscar.cbls.modeling.routing.VRS
import oscar.cbls.test.invBench.{InvTestBenchWithConstGen, TestBenchSut}

class GlobalCapacityConstraintTests extends AnyFunSuite with Matchers {

  def generateCapacityConstraintData(vrs: VRS): (Array[Long], Array[Long], Array[Long]) = {
    val vehicleCapacity = Array.fill(vrs.v)(20L)
    val contentVariationAtNode = Array.tabulate(vrs.n)(node => {
      if (node < vrs.v) 2L
      else if (node % 2 == 0) 2L
      else -2L
    })
    val contentVariationBackAtDepot = Array.fill(vrs.v)(-2L)
    (vehicleCapacity, contentVariationAtNode, contentVariationBackAtDepot)
  }

  test("GlobalCapacityConstraint: initialization errors are properly caught.") {
    val model = new Store(debugLevel = 3)
    val vrs   = VRS(model, 30, 2)

    var exception =
      intercept[IllegalArgumentException](
        GlobalCapacityConstraintWithLogReduction(
          vrs,
          Array(10L, -10L),
          Array.fill(vrs.n)(0L),
          Some(Array.fill(vrs.v)(0L))
        )
      )
    assert(exception.getMessage.contains("Vehicle capacity can not be negative."))

    exception = intercept[IllegalArgumentException](
      GlobalCapacityConstraintWithLogReduction(
        vrs,
        Array(10L, 10L),
        Array.fill(vrs.n)(-1L),
        Some(Array.fill(vrs.v)(0L))
      )
    )
    assert(exception.getMessage.contains("Vehicle can not start with a negative content."))

  }

  test("GlobalCapacityConstraint: initialization works as expected.") {
    val model                                          = new Store(debugLevel = 3)
    val vrs                                            = VRS(model, 30, 2)
    val (capacity, contentVariation, contentBackDepot) = generateCapacityConstraintData(vrs)
    val inv = GlobalCapacityConstraintWithLogReduction(
      vrs,
      capacity,
      contentVariation,
      Some(contentBackDepot)
    )
    model.close()

    inv(0).value() must be(0)
    inv(1).value() must be(0)
  }

  test("GlobalCapacityConstraint: Assign works and no violation is detected.") {
    val model                                          = new Store(debugLevel = 3)
    val vrs                                            = VRS(model, 30, 2)
    val (capacity, contentVariation, contentBackDepot) = generateCapacityConstraintData(vrs)
    val inv = GlobalCapacityConstraintWithLogReduction(
      vrs,
      capacity,
      contentVariation,
      Some(contentBackDepot)
    )
    model.close()
    vrs.routes := IntSequence(
      List(0) ::: List.from(2 until 12) ::: List(1) ::: List.from(12 until 24)
    )
    model.propagate()

    inv(0).value() must be(0) // Picking up goods and delivery them directly after.
    inv(1).value() must be(0) // Picking up goods and delivery them directly after.
  }

  test("GlobalCapacityConstraint: Assign works and violation is detected if needed.") {
    val model                                          = new Store(debugLevel = 3)
    val vrs                                            = VRS(model, 30, 2)
    val (capacity, contentVariation, contentBackDepot) = generateCapacityConstraintData(vrs)
    val inv = GlobalCapacityConstraintWithLogReduction(
      vrs,
      capacity,
      contentVariation,
      Some(contentBackDepot)
    )
    model.close()
    vrs.routes := IntSequence(
      List(0) ::: List.from(2 until 29 by 2) ::: List(1) ::: List.from(3 to 29 by 2)
    )
    model.propagate()

    inv(0).value() must be(1) // Exceeding capacity
    inv(1).value() must be(1) // Negative content
  }

  test("GlobalCapacityConstraint: Define checkpoint and simple insertion works as expected.") {
    val model                                          = new Store(debugLevel = 3)
    val vrs                                            = VRS(model, 30, 2)
    val (capacity, contentVariation, contentBackDepot) = generateCapacityConstraintData(vrs)
    val inv = GlobalCapacityConstraintWithLogReduction(
      vrs,
      capacity,
      contentVariation,
      Some(contentBackDepot)
    )
    model.close()
    vrs.routes.defineCurrentValueAsCheckpoint()
    vrs.routes.insertAfterPosition(2, vrs.routes.value().explorerAtPosition(0).get)
    vrs.routes.insertAfterPosition(3, vrs.routes.value().explorerAtPosition(2).get)

    inv(0).value() must be(0)
    inv(1).value() must be(1) // Negative content
  }

  test("GlobalCapacityConstraint: Empty route with negative back to depot content is tolerated") {
    val model                                          = new Store(debugLevel = 3)
    val vrs                                            = VRS(model, 30, 2)
    val (capacity, contentVariation, contentBackDepot) = generateCapacityConstraintData(vrs)
    val inv = GlobalCapacityConstraintWithLogReduction(
      vrs,
      capacity,
      contentVariation,
      Some(contentBackDepot)
    )
    model.close()
    vrs.routes.defineCurrentValueAsCheckpoint()
    vrs.routes.insertAfterPosition(2, vrs.routes.value().explorerAtPosition(0).get)
    vrs.routes.insertAfterPosition(3, vrs.routes.value().explorerAtPosition(2).get)

    inv(0).value() must be(0)
    inv(1).value() must be(1) // Negative content
  }

  test("GlobalCapacityConstraint: test bench") {
    val n = 25
    val v = 5

    class GlobalCapacityConstraintTestBench(additionalSeeds: List[String] = List())
        extends InvTestBenchWithConstGen[(Array[Long], Array[Long], Array[Long])](
          "GlobalCapacityConstraint test bench",
          additionalSeeds
        ) {

      override def genConst(): Gen[(Array[Long], Array[Long], Array[Long])] = {
        for {
          capacity <- Gen.sequence[Array[Long], Long](Array.fill(v)(Gen.choose(20L, 50L)))
          contentVariation <- Gen.sequence[Array[Long], Long](
            Array.tabulate(n)(x => if (x < v) Gen.choose(0L, 2L) else Gen.choose(-2L, 2L))
          )
          backAtDepot <- Gen.sequence[Array[Long], Long](Array.fill(v)(Gen.choose(0L, 0L)))
        } yield (capacity, contentVariation, backAtDepot)
      }

      override def createTestBenchSut(
        model: Store,
        inputData: (Array[Long], Array[Long], Array[Long])
      ): TestBenchSut = {

        val vrs: VRS = VRS(model, n, v)
        val inv = GlobalCapacityConstraintWithLogReduction(
          vrs,
          inputData._1,
          inputData._2,
          Some(inputData._3),
          Some("GlobalCapacityConstraintWithLogReduction")
        )
        TestBenchSut(inv, Array(vrs.routes), inv(), Some(vrs))
      }

      override def typeTToString(elem: (Array[Long], Array[Long], Array[Long])): String = {
        s"Data:\nCapacity : ${elem._1.mkString(" - ")}\n" +
          s"Content variation : ${elem._2.mkString(" - ")}\n" +
          s"Content variation back at depot : ${elem._3.mkString(" - ")}"
      }
    }

    val bench = new GlobalCapacityConstraintTestBench()
    bench.test()
  }

}

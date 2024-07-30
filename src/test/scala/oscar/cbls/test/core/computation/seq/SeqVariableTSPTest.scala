package oscar.cbls.test.core.computation.seq

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.seq.SeqVariable

class SeqVariableTSPTest extends AnyFunSuite{

  private def generateVRPData(): (SeqVariable, SeqVariable) ={
    val model:  Store = new Store(3)
    val route: SeqVariable = new SeqVariable(model, List.empty, "myRoute")
    val copyForRouteLength: SeqVariable = route.createClone()
    model.close()
    (route, copyForRouteLength)
  }

  test(s"The 'one checkpoint' usage of a TSP works as expected"){
    val (route, copyForRouteLength) = generateVRPData()

    route.defineCurrentValueAsCheckpoint()
    route.insertAfterPosition(5,route.value.explorerAtPosition(-1).get)
    copyForRouteLength.value.toList should be(List(5))
    route.rollbackToTopCheckpoint()
    route.insertAfterPosition(8,route.value.explorerAtPosition(-1).get)
    copyForRouteLength.value.toList should be(List(8))
    route.rollbackToTopCheckpoint()
    route.releaseTopCheckpoint()
    route.insertAfterPosition(8,route.value.explorerAtPosition(-1).get)
    copyForRouteLength.value.toList should be(List(8))

    val exception =
      intercept[IllegalArgumentException](route.rollbackToTopCheckpoint())
    assert(
      exception.getMessage
        .contains("Can not rollback to top checkpoint since no checkpoint has been defined"))
  }

  test(s"The 'two checkpoint' usage of a TSP works as expected"){
    val (route, copyForRouteLength) = generateVRPData()

    // Testing inserting 5 and then 8 (after and before 5)
    route.defineCurrentValueAsCheckpoint()
    route.insertAfterPosition(5,route.value.explorerAtPosition(-1).get)
    route.defineCurrentValueAsCheckpoint()
    route.insertAfterPosition(8,route.value.explorerAtPosition(0).get)
    copyForRouteLength.value.toList should be(List(5,8))
    route.rollbackToTopCheckpoint()
    route.insertAfterPosition(8,route.value.explorerAtPosition(-1).get)
    copyForRouteLength.value.toList should be(List(8,5))
    route.rollbackToTopCheckpoint()
    route.releaseTopCheckpoint()
    route.rollbackToTopCheckpoint()

    // Testing and validating insertion of 4 and then 7 (after and before 4)
    route.insertAfterPosition(4,route.value.explorerAtPosition(-1).get)
    route.defineCurrentValueAsCheckpoint()
    route.insertAfterPosition(7,route.value.explorerAtPosition(0).get)
    copyForRouteLength.value.toList should be(List(4,7))
    route.rollbackToTopCheckpoint()
    route.insertAfterPosition(7,route.value.explorerAtPosition(-1).get)
    copyForRouteLength.value.toList should be(List(7,4))
    route.rollbackToTopCheckpoint()
    route.releaseTopCheckpoint()
    route.rollbackToTopCheckpoint()
    route.releaseTopCheckpoint()
    route.insertAfterPosition(4,route.value.explorerAtPosition(-1).get)
    route.insertAfterPosition(7,route.value.explorerAtPosition(-1).get)
    copyForRouteLength.value.toList should be(List(7,4))

    var exception =
      intercept[IllegalArgumentException](route.rollbackToTopCheckpoint())
    assert(
      exception.getMessage
        .contains("Can not rollback to top checkpoint since no checkpoint has been defined"))

    exception =
      intercept[IllegalArgumentException](copyForRouteLength.rollbackToTopCheckpoint())
    assert(
      exception.getMessage
        .contains("Can not rollback to top checkpoint since no checkpoint has been defined"))
  }

}

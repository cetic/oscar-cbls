package oscar.cbls.test.algo

import org.scalatest._
import oscar.cbls._
import oscar.cbls.core.computation.{CBLSIntVar, DomainRange}
import oscar.cbls.lib.search.neighborhoods.{NarrowingExhaustive, TransferNeighborhood}

class testTransferWithLO extends FlatSpec {

  val s = new Store()
  val a = 1
  val b = 2
  val c = 1

  val var1InitValue = 0
  val var2InitValue = 100


  val var1 = CBLSIntVar(s,var1InitValue,DomainRange(-1000,1000))
  val var2 = CBLSIntVar(s,var2InitValue,DomainRange(-1000,1000))

  val obj = new IntVarObjective(var1 * var1 * a + var1 * b + c)

  s.close()

  val search = TransferNeighborhood(Array(var1,var2),searchZoneForDelta = () => (_,_) => (_,_) => new NarrowingExhaustive(100,101))

  search.doAllMoves(obj = obj)

//  "The result the search for var1" should "be where the derivate is null" in {
//    assert(var1.value == 0 - b/(2 * a))
//  }
  "The delta" should "be cut off to var2" in {
    assert(var2.value == var2InitValue - (var1.value - var1InitValue))
  }


}

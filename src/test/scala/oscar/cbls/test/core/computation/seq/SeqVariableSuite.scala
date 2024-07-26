package oscar.cbls.test.core.computation.seq

import org.scalacheck.{Properties, Test}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import oscar.cbls.algo.sequence.Token
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.seq.SeqVariable

import scala.util.Random

class SeqVariableProperties extends Properties("SeqVariable Properties") {

//  propertyWithSeed("Commands work, Explicit seed", Some("Zj24rqSBRvGwY8WHPUrurk4CXbA9BuvS6Gb-5owQNmN=")) = SeqVariableCommands.property()
  propertyWithSeed("Commands work, no Explicit seed", None) = SeqVariableCommands.property()

  override def overrideParameters(p: Test.Parameters): Test.Parameters = {
    p.withMinSuccessfulTests(500)
  }
}

class SeqVariableSuite extends AnyFunSuite with Checkers {
  val seqVariableProperty = new SeqVariableProperties
  seqVariableProperty.check()
}
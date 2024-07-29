package oscar.cbls.test.core.computation.seq

import org.scalacheck.{Properties, Test}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import oscar.cbls.algo.sequence.Token
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.seq.SeqVariable

import scala.util.Random

class SeqVariableProperties(initialSize: Int) extends Properties("SeqVariable Properties") {

//  propertyWithSeed("Commands work, Explicit seed", Some("A_NJIYquVIQ95ha1eB3F05kDdyE2hXu9u3JzjSyjq_C=")) = SeqVariableCommands.property()

  SeqVariableCommands.setSize(initialSize)
  propertyWithSeed(s"Commands work starting with sequence of size $initialSize, no Explicit seed", None) = SeqVariableCommands.property()

  override def overrideParameters(p: Test.Parameters): Test.Parameters = {
    p.withMinSuccessfulTests(500)
  }
}

class SeqVariableSuite extends AnyFunSuite with Checkers {
  new SeqVariableProperties(0).check()
  new SeqVariableProperties(20).check()
}
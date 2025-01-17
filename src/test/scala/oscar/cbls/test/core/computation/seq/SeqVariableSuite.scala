package oscar.cbls.test.core.computation.seq

import org.scalacheck.{Properties, Test}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers

class NonEmptySeqVariableProperties extends Properties("SeqVariable Properties") {

  SeqVariableCommands.setSize(100)
  propertyWithSeed(s"Commands work starting with sequence of size 100, no Explicit seed", None) =
    SeqVariableCommands.property()

  override def overrideParameters(p: Test.Parameters): Test.Parameters = {
    p.withMinSuccessfulTests(500)
  }
}

class EmptySeqVariableProperties extends Properties("SeqVariable Properties") {

  propertyWithSeed(s"Commands work starting with empty sequence, no Explicit seed", None) =
    SeqVariableCommands.property()

  override def overrideParameters(p: Test.Parameters): Test.Parameters = {
    p.withMinSuccessfulTests(500)
  }
}

class SeqVariableSuite extends AnyFunSuite with Checkers {
  new NonEmptySeqVariableProperties().check()
  new EmptySeqVariableProperties().check()
}

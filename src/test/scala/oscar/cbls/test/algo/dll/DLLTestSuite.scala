package oscar.cbls.test.algo.dll

import org.scalacheck.{Properties, Test}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers

class DLLProperties extends Properties("DLL Properties") {

  propertyWithSeed("Commands work, no Explicit seed", None) = DLLTestCommands.property()

  override def overrideParameters(p: Test.Parameters): Test.Parameters = {
    p.withMinSuccessfulTests(500)
  }

}

class OperationTest extends AnyFunSuite with Checkers {

  val dllProperty = new DLLProperties

  dllProperty.check()

}

package oscar.cbls.test.algo.dll

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import oscar.cbls.algo.dll.DoublyLinkedList

import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.util.ConsoleReporter
import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalatest.prop.Configuration
import org.scalacheck.Test

class DLLProperties extends Properties("DLL Properties") {

  propertyWithSeed("Commands work, no Explicit seed", None) = DLLTestCommands.property()

  override def overrideParameters(p: Test.Parameters): Test.Parameters = {
    p.withMinSuccessfulTests(500)
  }

}

class OperationTest extends AnyFunSuite with Checkers {

  import org.scalacheck.Test

  val dllProperty = new DLLProperties

  dllProperty.check()

}

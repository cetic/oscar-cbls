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

  val seedList = List("OA5iYY0IknAd0ztukxU_GjVq6Am8i_CImCnRYr5vHYH=",
    "0R2Vp7iQ7dzDrp6KaoPfmqLZB0yGCAelv6ujE_h-fJD="
  )

  seedList.distinct.foreach(s => {
    propertyWithSeed(s"DLL command list works (seed:  $s)",Some(s)) = DLLTestCommands.property()
  })


  propertyWithSeed("Commands work, no Explicit seed",None) =
    DLLTestCommands.property()

}

class OperationTest extends AnyFunSuite with Checkers{

  import org.scalacheck.Test

  val dllProperty = new DLLProperties

  dllProperty.check()


}

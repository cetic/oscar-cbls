package oscar.ml.spm


import org.scalatest._
import oscar.cp._

class PPICTest extends FlatSpec with Matchers {

  "PPIC test" should "should be correct" in {

    val s1 = Array(1, 2, 3, 2, 1)
    val s2 = Array(2, 1, 2, 3)
    val s3 = Array(1, 2)
    val s4 = Array(2, 3, 4)

    val db = Array(s1, s2, s3, s4)

    implicit val cp = CPSolver()

    val P = Array.fill(4)(CPIntVar(0 to 4))

    add(new PPIC(P,db,3))

  }

}

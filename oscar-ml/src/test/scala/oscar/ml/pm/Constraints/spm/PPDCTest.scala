package oscar.ml.pm.Constraints.spm

import oscar.cp.core.CPSolver
import oscar.cp.core.variables.CPIntVar
import oscar.cp.testUtils.TestSuite
import oscar.ml.pm.utils.{DatasetUtils, SpmfWithHeaderFormat, TestHelpers}

class PPDCTest extends TestSuite {
  test("SPM Leviathan (sup = 20%)") {
    case class Config(
                       path: String = "oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test/",
                       filename: String = "input/leviathan_with_items.txt.spmf",
                       solsFilename: String = "output/leviathan_prefixspan_20p.txt",
                       minsup: Double = 0.20,
                       verbose: Boolean = true,
                       timeLimit: Int = 5
                     )


    val config = Config()
    val epsilon: Int = 0
    val (db, frequency, nTrans, nItems, lenSeqMax, freqentItems) =  DatasetUtils.prepareForSPM(config.path + config.filename, config.minsup, SpmfWithHeaderFormat)
    val domS = epsilon +: freqentItems
    val output = TestHelpers.readSols(config.path + config.solsFilename)


    System.err.println(config + s"\nSup: $frequency\nnItems: $nItems\nnTrans: $nTrans")

    // Initializing the solver
    implicit val cp = CPSolver()

    // Declaring variables
    val P = Array.fill(lenSeqMax)(CPIntVar.sparse(domS)(cp))

    // Posting constraints
    cp.add(P(0) > epsilon)

    val constraint = new PPDC(P, frequency, db)
    cp.add(constraint)

    // Searching for solutions
    cp.search(oscar.cp.binaryStatic(P))

    cp.onSolution {
      val actualSolKey = db.patternToString(P.map(_.min).filter(_ > 0))
      println(actualSolKey)
      assert(output(actualSolKey) == constraint.curPrefixSupport)
    }

    val statsActual = cp.start()

    assert(statsActual.nSols == output.size)

    System.err.println(statsActual)

  }
}

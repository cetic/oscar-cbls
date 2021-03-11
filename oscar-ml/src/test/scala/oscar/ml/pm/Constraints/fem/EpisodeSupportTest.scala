package oscar.ml.pm.Constraints.fem

import oscar.cp.core.CPSolver
import oscar.cp.core.variables.CPIntVar
import oscar.cp.testUtils.TestSuite
import oscar.ml.pm.Constraints.spm.PPIC
import oscar.ml.pm.utils.{DatasetUtils, SpmfFormat, TestHelpers}

class EpisodeSupportTest extends TestSuite {
  test("FEM check solution (sup = 2)") {
    case class Config(
                       path: String = "oscar-ml/src/main/scala/oscar/ml/pm/data/fem/test/",
                       filename: String = "input/test.fasta",
                       solsFilename: String = "output/test_fasta_sols_2.txt",
                       minsup: Double = 2,
                       verbose: Boolean = true,
                       timeLimit: Int = 5
                     )


    val config = Config()
    val epsilon: Int = 0
    val (db, frequency, nTrans, nItems, lenSeqMax, freqentItems) = DatasetUtils.prepareForFEM(config.path + config.filename, config.minsup)
    val domS = epsilon +: freqentItems
    val output = TestHelpers.readSols(config.path + config.solsFilename)

    // Initializing the solver
    implicit val cp = CPSolver()

    // Declaring variables
    val P = Array.fill(lenSeqMax)(CPIntVar.sparse(domS)(cp))

    // Posting constraints
    cp.add(P(0) > epsilon)

    val constraint = new EpisodeSupport(P, frequency, db)

    cp.add(constraint)

    // Searching for solutions
    cp.search(oscar.cp.binaryStatic(P))

    cp.onSolution {
      val actualSolKey = db.patternToString(P.map(_.min).filter(_ > 0))
      assert(output(actualSolKey) == constraint.curPrefixSupport)
    }

    val statsActual = cp.start()

    assert(statsActual.nSols == output.size)

  }
}

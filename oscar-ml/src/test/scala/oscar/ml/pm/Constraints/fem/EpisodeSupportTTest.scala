package oscar.ml.pm.Constraints.fem

import oscar.cp.core.CPSolver
import oscar.cp.core.variables.CPIntVar
import oscar.cp.testUtils.TestSuite
import oscar.ml.pm.utils.{DatasetUtils, LongSequenceWithNameAndTime, TestHelpers, TimeOption}

class EpisodeSupportTTest extends TestSuite {

  test("FEM Time check solution (sup = 1) gap[2, 7] span[1, 10]") {
    case class Config(
                       path: String = "oscar-ml/src/main/scala/oscar/ml/pm/data/fem/test/",
                       filename: String = "input/test.ubilog.name.time.txt",
                       solsFilename: String = "output/test_ubilog_sols_1_1_10_2_7.txt",
                       minsup: Double = 1,
                       verbose: Boolean = true,
                       timeLimit: Int = 5,
                       timeOption: TimeOption = TimeOption(minspan = 1, maxspan = 10, mingap = 2, maxgap = 7)
                     )


    val config = Config()
    val epsilon: Int = 0
    val (db, frequency, nTrans, nItems, lenSeqMax, freqentItems) = DatasetUtils.prepareForFEMTime(config.path + config.filename, config.minsup, LongSequenceWithNameAndTime)
    val domS = epsilon +: freqentItems
    val output = TestHelpers.readSols(config.path + config.solsFilename)

    // Initializing the solver
    implicit val cp = CPSolver()

    // Declaring variables
    val P = Array.fill(lenSeqMax)(CPIntVar.sparse(domS)(cp))

    // Posting constraints
    cp.add(P(0) > epsilon)

    val constraint = new EpisodeSupportT(P, frequency, db, config.timeOption)

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

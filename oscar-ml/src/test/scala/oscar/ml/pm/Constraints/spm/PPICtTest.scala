package oscar.ml.pm.Constraints.spm

import oscar.cp.core.CPSolver
import oscar.cp.core.variables.CPIntVar
import oscar.cp.testUtils.TestSuite
import oscar.ml.pm.utils.{DatasetUtils, SpmfFormat, SpmfWithHeaderFormat, SpmfWithTimeFormat, TestHelpers, TimeOption}

class PPICtTest extends TestSuite {

  /**
   * Comparing PPICt with HirateYamana (spmf implem: 50% span(0, 2), gap(0, 2):
   * Yu Hirate, Hayato Yamana (2006) Generalized Sequential Pattern Mining with Item Intervals. JCP 1(3): 51-60.
   */
  test("SPM with time check solution (sup = 2)") {
    case class Config(
                       path: String = "oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test/",
                       filename: String = "input/test.postime.spmf",
                       solsFilename: String = "output/test_HirateYamana_2_02_02.txt",
                       minsup: Double = 0.5,
                       verbose: Boolean = true,
                       timeLimit: Int = 5
                     )


    val config = Config()
    val epsilon: Int = 0
    val (db, frequency, nTrans, nItems, lenSeqMax, freqentItems, maxtime) =  DatasetUtils.prepareForSPMTime(config.path + config.filename, config.minsup, SpmfWithTimeFormat)
    val domS = epsilon +: freqentItems
    val output = TestHelpers.readSols(config.path + config.solsFilename)

    // Initializing the solver
    implicit val cp = CPSolver()

    // Declaring variables
    val P = Array.fill(lenSeqMax)(CPIntVar.sparse(domS)(cp))

    // Posting constraints
    cp.add(P(0) > epsilon)

    val constraint = new PPICt(P, frequency, db, TimeOption(0, 2, 0, 2))
    cp.add(constraint)

    // Searching for solutions
    cp.search(oscar.cp.binaryStatic(P))

    cp.onSolution {
      val actualSolKey = P.map(_.min).filter(_ > 0).mkString(" ")
      assert(output(actualSolKey) <= constraint.curPrefixSupport)
    }

    val statsActual = cp.start()

    assert(statsActual.nSols == output.size)


  }


  /**
   * PPICt is equal to PPIC if time constraints are not specified
   */
  test("PPIC == PPICt") {
    case class Config(
                       path: String = "oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test/",
                       filename: String = "input/test.time.spmf",
                       solsFilename: String = "output/test_prefixspan_2.txt",
                       minsup: Double = 0.5,
                       verbose: Boolean = true,
                       timeLimit: Int = 5
                     )


    val config = Config()
    val epsilon: Int = 0
    val (db, frequency, nTrans, nItems, lenSeqMax, freqentItems, maxtime) =  DatasetUtils.prepareForSPMTime(config.path + config.filename, config.minsup, SpmfWithTimeFormat)
    val domS = epsilon +: freqentItems
    val output = TestHelpers.readSols(config.path + config.solsFilename)

    // Initializing the solver
    implicit val cp = CPSolver()

    // Declaring variables
    val P = Array.fill(lenSeqMax)(CPIntVar.sparse(domS)(cp))

    // Posting constraints
    cp.add(P(0) > epsilon)

    val constraint = new PPICt(P, frequency, db, TimeOption(0, maxtime+1, 1, maxtime+1)) //TimeOption(0, maxtime+1, 3, 7)
    cp.add(constraint)

    // Searching for solutions
    cp.search(oscar.cp.binaryStatic(P))

    cp.onSolution {
      val actualSolKey = P.map(_.min).filter(_ > 0).mkString(" ")
      assert(output(actualSolKey) == constraint.curPrefixSupport)
    }

    val statsActual = cp.start()

    assert(statsActual.nSols == output.size)
  }

}

/**
 * Run an example of FIM problem solved in pure CP (find dataset examples in data/fim)
 *
 * @author John Aoga johnaoga@gmail.com
 *
 */

package oscar.ml.pm.examples.fem

import oscar.cp._
import oscar.ml.pm.Constraints.fem.EpisodeSupport
import oscar.ml.pm.Constraints.spm.PPIC
import oscar.ml.pm.utils.DatasetUtils

object FEMRunner extends App {

  case class Config(
                     filename: String = "oscar-ml/src/main/scala/oscar/ml/pm/data/fem/test/input/test.fasta",
                     minsup: Double = 2,
                     verbose: Boolean = true,
                     timeLimit: Int = 5
                   )

  printHead()

  val config = Config()
  val epsilon: Int = 0
  val (db, frequency, nTrans, nItems, lenSeqMax, freqentItems) =  DatasetUtils.prepareForFEM(config.filename, config.minsup)
  val domS = epsilon +: freqentItems

  ///println(db)

  System.err.println(config + s"\nSup: $frequency\nnItems: $nItems\nnTrans: $nTrans")

  if (lenSeqMax > 0) {
    // Initializing the solver
    implicit val cp = CPSolver()

    // Declaring variables
    val P = Array.fill(lenSeqMax)(CPIntVar.sparse(domS)(cp))

    // Posting constraints
    cp.add(P(0) > epsilon)

    val constraint = new EpisodeSupport(P, frequency, db)

    cp.add(constraint)

    // Searching for solutions
    cp.search(binaryStatic(P))

    // Dsplaying solutions
    if (config.verbose) {
      cp.onSolution {
        println(db.patternToString(P.map(_.min).filter(_ > 0)) + " #SUP: " + constraint.curPrefixSupport)

      }
    }

    // Running the solver (with time limit set to 1000s)
    if (config.timeLimit > 0) {
      System.err.println(cp.start(timeLimit = config.timeLimit))
    } else {
      System.err.println(cp.start())
    }
  } else System.err.println("No solution")

  //Misc
  def printHead(): Unit = {
    System.err.println(
      """
    /** FEM with a global constraint - Episode Support v1.0
    Bugs reports : johnaoga@gmail.com , quentin.cappart@uclouvain.be
    */
      """)
  }
}

/**
 *
Config(oscar-ml/src/main/scala/oscar/ml/pm/data/fem/test/input/test.fasta,2.0,true,5)

Sup: 2
nItems: 4
nTrans: 6

A #SUP: 3
A A #SUP: 2
A A B #SUP: 2
A B #SUP: 3
B #SUP: 2

nNodes: 8
nFails: 5
time(ms): 9
completed: true
timeInTrail: 0
nSols: 5
 */
/**
 * Run an example of FIM problem solved in pure CP (find dataset examples in data/fim)
 *
 * @author John Aoga johnaoga@gmail.com
 *
 */

package oscar.ml.pm.examples.spm

import oscar.cp._
import oscar.ml.pm.Constraints.spm.{PPIC, PPDC, PPmixed}
import oscar.ml.pm.utils.DatasetUtils

object SPMRunner extends App {

  case class Config(
                     filename: String = "oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test2.data",
                     minsup: Double = 0.5,
                     verbose: Boolean = true,
                     timeLimit: Int = 5
                   )

  printHead()

  val config = Config()
  val epsilon: Int = 0
  val (db, frequency, nTrans, nItems, lenSeqMax, freqentItems) =  DatasetUtils.prepareForSPM(config.filename, config.minsup)
  val domS = epsilon +: freqentItems

  System.err.println(config + s"\nSup: $frequency\nnItems: $nItems\nnTrans: $nTrans")

  if (lenSeqMax > 0 && nTrans >= frequency) {
    // Initializing the solver
    implicit val cp = CPSolver()

    // Declaring variables
    val P = Array.fill(lenSeqMax)(CPIntVar.sparse(domS)(cp))

    // Posting constraints
    cp.add(P(0) > epsilon)

    val constraint = new PPIC(P, frequency, db)
    //val constraint = new PPDC(P, frequency, db) // Alternative to PPIC (slower)
    //val constraint = new PPmixed(P, frequency, db) // Alternative to PPIC (slower)
    cp.add(constraint)

    // Searching for solutions
    cp.search(binaryStatic(P))

    // Dsplaying solutions
    if (config.verbose) {
      cp.onSolution {
        println(P.map(_.min).filter(_ > 0).mkString(" ")+" #SUP: "+constraint.curPrefixSupport)
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
    /** SPM with a global constraint - PP[[I, D]C, mixed] v1.0
    Bugs reports : johnaoga@gmail.com , pschaus@gmail.com
    */
      """)
  }
}

/**
 *
Config(oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test2.data,0.5,true,5)
Sup: 2
nItems: 4
nTrans: 4

1 #SUP: 3
1 2 #SUP: 3
1 2 3 #SUP: 2
1 3 #SUP: 2
2 #SUP: 4
2 2 #SUP: 2
2 2 3 #SUP: 2
2 3 #SUP: 3
3 #SUP: 3

nNodes: 16
nFails: 9
time(ms): 8
completed: true
timeInTrail: 0
nSols: 9
 */
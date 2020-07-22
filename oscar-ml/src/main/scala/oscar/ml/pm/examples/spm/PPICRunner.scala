/**
 * Run an example of FIM problem solved in pure CP (find dataset examples in data/fim)
 *
 * @author John Aoga johnaoga@gmail.com
 *
 */

package oscar.ml.pm.examples.spm

import oscar.cp._
import oscar.ml.pm.Constraints.fim.CoverSize
import oscar.ml.pm.Constraints.spm.PPIC
import oscar.ml.pm.utils.{Dataset, DatasetUtils}

object PPICRunner extends App {

  case class Config(
                     filename: String = "oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test2.data",
                     minsup: Double = 0.75,
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
    /** SPM with a global constraint - PPIC v1.0
    Bugs reports : johnaoga@gmail.com , pschaus@gmail.com
    */
      """)
  }
}
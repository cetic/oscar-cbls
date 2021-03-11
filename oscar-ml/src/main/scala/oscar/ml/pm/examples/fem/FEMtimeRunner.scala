/**
 * Run an example of FIM problem solved in pure CP (find dataset examples in data/fim)
 *
 * @author John Aoga johnaoga@gmail.com
 *
 */

package oscar.ml.pm.examples.fem

import oscar.cp._
import oscar.ml.pm.Constraints.fem.{EpisodeSupport, EpisodeSupportT}
import oscar.ml.pm.utils.{DatasetUtils, LongSequenceWithNameAndTime, TimeOption}

/**
  * Find frequent episodes in a Ubiqlog such that:
  * - support threshold: 1
  * - gap[2,7] Span[1,10]
  * > Expected nSols = 22
 */
object FEMtimeRunner extends App {

  case class Config(
                     filename: String = "oscar-ml/src/main/scala/oscar/ml/pm/data/fem/test/input/test.ubilog.name.time.txt",
                     minsup: Double = 1,
                     verbose: Boolean = true,
                     timeLimit: Int = 5,
                     timeOption: TimeOption = TimeOption(minspan = 1, maxspan = 10, mingap = 2, maxgap = 7)
                   )

  printHead()

  val config = Config()
  val epsilon: Int = 0
  val (db, frequency, nTrans, nItems, lenSeqMax, freqentItems) =  DatasetUtils.prepareForFEMTime(config.filename, config.minsup, LongSequenceWithNameAndTime)
  val domS = epsilon +: freqentItems

  //println(db)

  System.err.println(config + s"\nSup: $frequency\nnItems: $nItems\nnTrans: $nTrans")

  if (lenSeqMax > 0) {
    // Initializing the solver
    implicit val cp = CPSolver()

    // Declaring variables
    val P = Array.fill(lenSeqMax)(CPIntVar.sparse(domS)(cp))

    // Posting constraints
    cp.add(P(0) > epsilon)

    val constraint = new EpisodeSupportT(P, frequency, db, config.timeOption)

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
    /** FEM + time with a global constraint - Episode Support Time v1.0
    Bugs reports : johnaoga@gmail.com , quentin.cappart@uclouvain.be
    */
      """)
  }
}

/**
 *
Config(oscar-ml/src/main/scala/oscar/ml/pm/data/fem/test/input/test.ubilog.name.time.txt,1.0,true,5)

Sup: 1
nItems: 4
nTrans: 7

A #SUP: 3
A A #SUP: 2
A A A #SUP: 1
A A B #SUP: 2
A B #SUP: 3
A B A #SUP: 1
A B A A #SUP: 1
A B A B #SUP: 1
A B B #SUP: 2
A B C #SUP: 1
A B C A #SUP: 1
A C #SUP: 1
A C A #SUP: 1
B #SUP: 3
B A #SUP: 1
B A A #SUP: 1
B A B #SUP: 1
B B #SUP: 2
B C #SUP: 1
B C A #SUP: 1
C #SUP: 1
C A #SUP: 1

nNodes: 58
nFails: 30
time(ms): 9
completed: true
timeInTrail: 1
nSols: 22
 */
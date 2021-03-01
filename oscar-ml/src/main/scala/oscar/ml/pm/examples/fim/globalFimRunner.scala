/**
 * Run an example of FIM problem solved in pure CP (find dataset examples in data/fim)
 *
 * @author John Aoga johnaoga@gmail.com
 *
 */

package oscar.ml.pm.examples.fim

import oscar.cp._
import oscar.ml.pm.Constraints.fim.FIM
import oscar.ml.pm.utils.Dataset

object globalFimRunner extends App {

  case class Config(
                     filename: String = "oscar-ml/src/main/scala/oscar/ml/pm/data/fim/mushroom.txt",
                     minsup: Double = 0.10,
                     verbose: Boolean = false,
                     timeLimit: Int = 1000
                   )

  printHead()

  val config = Config()
  val db = Dataset(config.filename)
  val tdbVertical = db.intoVertical()
  val nTrans = db.nbTrans
  val nItems = db.nbItem
  var frequency = config.minsup.intValue()

  if (config.minsup > 0 && config.minsup < 1) frequency = (config.minsup * nTrans).ceil.toInt //floor is another way around for the support

  // Initializing the solver
  implicit val cp = CPSolver()

  // Declaring variables
  val I = Array.fill(nItems)(CPBoolVar()(cp))
  //val freqVar = CPIntVar(frequency to frequency)(cp)

  // Posting constraints
  cp.add(new FIM(I, frequency, db))

  // Searching for solutions
  /// sorting item by support
  val Isorted = I.indices.sortBy(tdbVertical(_).size).map(I(_)).toArray

  /// search
  cp.search(binaryStatic(Isorted))

  // Dsplaying solutions
  if (config.verbose) {
    cp.onSolution {
      print(">\t")
      var i = 0
      while (i < I.length) {
        if (I(i).min == 1)
          print(i + ",")
        i += 1
      }
      println()
    }
  }

  // Running the solver (with time limit set to 1000s)
  if (config.timeLimit > 0) {
    println(cp.start(timeLimit = config.timeLimit))
  } else {
    println(cp.start())
  }

  //Misc
  def printHead(): Unit = {
    System.err.println(
      """
    /** FIM with a single global constraint (OscaR Solver) v1.0
    Bugs reports : johnaoga@gmail.com , pschaus@gmail.com
    */
      """)
  }
}
/**
 * Run an example of FIM problem solved in pure CP (find dataset examples in data/fim)
 *
 * @author John Aoga johnaoga@gmail.com
 *
 */

package oscar.ml.pm.examples.fim

import oscar.cp._
import oscar.cp.constraints.OrReif
import oscar.ml.pm.utils.Dataset


object pureFimRunner extends App {

  case class Config(
                     //"oscar-ml/src/main/scala/oscar/ml/pm/data/mushroom.txt",
                     filename: String = "oscar-ml/src/main/scala/oscar/ml/pm/data/fim/test.txt",
                     minsup: Double = 0.40,
                     verbose: Boolean = true,
                     timeLimit: Int = -1
                   )

  printHead()

  val config = Config()
  val db = Dataset(config.filename)
  val tdbVertical = db.intoVertical()
  val nTrans = db.nbTrans
  val nItems = db.nbItem
  var frequency = config.minsup.intValue()

  if (config.minsup > 0 && config.minsup < 1) frequency = (config.minsup * nTrans).ceil.toInt //floor is another way around for the support

  println(frequency)
  println(db.benchmarkName)

  // Initializing the solver
  implicit val cp = CPSolver()

  // Declaring variables
  val I = Array.fill(nItems)(CPBoolVar()(cp))
  val T = Array.tabulate(nTrans)(t => CPBoolVar()(cp))
  val freqVar = CPIntVar(frequency to nTrans)(cp)

  // Posting constraints

  /// Frequency constraint
  cp.add(sum(T) >= freqVar)

  /// Cover constraint
  for (t <- 0 until nTrans) {
    val tNotItems = (0 until nItems).filter(i => !tdbVertical(i).contains(t))
    cp.post(new OrReif(tNotItems.map(I(_)), !T(t)))
  }

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

      print(" #SUP = " + T.map(_.min).filter(_ == 1).length)

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
    /**  FIM with pure CP (OscaR Solver) - decomposition approach v1.0
    Bugs reports : johnaoga@gmail.com , pschaus@gmail.com
    */
      """)
  }

}


///output with "oscar-ml/src/main/scala/oscar/ml/pm/data/test.txt"
/*
>	 #SUP = 5
>	2, #SUP = 5
>	4, #SUP = 4
>	2,4, #SUP = 4
>	3, #SUP = 3
>	2,3, #SUP = 3
>	3,4, #SUP = 2
>	2,3,4, #SUP = 2
>	1, #SUP = 3
>	1,2, #SUP = 3
>	1,4, #SUP = 3
>	1,2,4, #SUP = 3
*/

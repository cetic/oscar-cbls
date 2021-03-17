/**
 * Run an example of CoverZize+ZDC for Discriminative or correlated problem (find db example in data/discriminative/-.txt)
 *
 * @author John Aoga johnaoga@gmail.com
 * @author Pierre Schaus pschaus@gmail.com
 *         Relevant paper: http://becool.info.ucl.ac.be/biblio/coversize-global-constraint-frequency-based-itemset-mining
 *
 */

package oscar.ml.pm.examples.fim

import oscar.cp._
import oscar.ml.pm.Constraints.fim.{CoverSize, ZDC}
import oscar.ml.pm.utils.{Dataset, TdbWithLabelFormat, ZDCScaled}

object DiscriminativeCoverSizeZDCRunner extends App {

  case class Config(
                     //"oscar-ml/src/main/scala/oscar/ml/pm/data/fim/test.txt",
                     filename: String = "oscar-ml/src/main/scala/oscar/ml/pm/data/fim/withlabels/mushroom.txt",
                     verbose: Boolean = true,
                     timeLimit: Int = -1
                   )

  printHead()

  val config = Config()
  val db = Dataset(config.filename, TdbWithLabelFormat)
  val vTdb = db.intoVertical()
  val (dbP, dbN) = db.splitDatasetByTwo()
  val nTrans = db.nbTrans
  val nItems = db.nbItem
  val zdc = new ZDCScaled(oscar.ml.pm.utils.InfGain, 100000)

  println(nTrans + " " + nItems)
  // Initializing the solver
  implicit val cp = CPSolver()

  // Declaring variables I , P, N, Minsup
  val I = Array.fill(nItems)(CPBoolVar()(cp))
  var P = CPIntVar(0 to dbP.nbTrans)(cp)
  var N = CPIntVar(0 to dbN.nbTrans)(cp)
  var score = CPIntVar(0 to 1000000)(cp)

  // Posting constraints: (CoverSize-) + (CoverSize+) + (ZDC)
  cp.add(new CoverSize(I, P, dbP))

  cp.add(new CoverSize(I, N, dbN))

  ///Discriminative eval function, look at utils/ZDCFunction for the availability functions
  cp.add(new ZDC(P, N, dbP.nbTrans, dbN.nbTrans, zdc, score))

  //COP, optimum
  maximize(score)

  val Isorted: Array[CPIntVar] = I.indices.sortBy(vTdb(_).size).map(I(_)).toArray[CPIntVar]

  search(binaryStatic(Isorted))

  onSolution {
    println("items:" + (0 until nItems).filter(I(_).isTrue) + s" pos:$P/" + dbP.nbTrans + s" neg:$N/" + dbN.nbTrans + " score:" + score)
  }

  val stat = start()
  println(stat)


  //Misc
  def printHead(): Unit = {
    System.err.println(
      """
    /** Discriminative FIM (OscaR Solver) - coversize+ZDC v1.0
    Bugs reports : johnaoga@gmail.com , pschaus@gmail.com
    */
      """)
  }

}

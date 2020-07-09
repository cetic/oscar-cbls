package oscar.ml.pm.Constraints.fim

import oscar.cp.testUtils.TestSuite
import oscar.cp.{CPBoolVar, CPSolver, binaryStatic}
import oscar.ml.pm.utils.Dataset

import scala.io.Source

class FIMTest extends TestSuite {
  def readSols(filename: String): Map[String, Int] =
    Source.fromFile(filename).getLines
      .map(line => line.split(" #SUP: "))
      .map { case Array(itemset, support) => itemset -> support.toInt }
      .toMap

  test("FIM check solution") {
    case class Config(
                       path: String = "oscar-ml/src/main/scala/oscar/ml/pm/data/fim/test/",
                       filename: String = "input/contextPasquier99.txt",
                       solsFilename: String = "output/contextPasquier99-eclat-2.txt",
                       minsup: Double = 0.40,
                       verbose: Boolean = false,
                       timeLimit: Int = 1000
                     )

    val config = Config()
    val db = Dataset(config.path + config.filename)
    val tdbVertical = db.intoVertical()
    val nTrans = db.nbTrans
    val nItems = db.nbItem
    var frequency = config.minsup.intValue()
    val output = readSols(config.path + config.solsFilename) + ("<>" -> nTrans)

    if (config.minsup > 0 && config.minsup < 1) frequency = (config.minsup * nTrans).ceil.toInt //floor is another way around for the support

    //System.err.println(config + s"\nSup: $frequency\nnItems: $nItems\nnTrans: $nTrans")

    // Initializing the solver
    implicit val cp = CPSolver()

    // Declaring variables
    val I = Array.fill(nItems)(CPBoolVar()(cp))

    val Isorted = I.indices.sortBy(tdbVertical(_).size).map(I(_)).toArray

    cp.search(binaryStatic(Isorted))

    // Posting constraints
    cp.add(new FIM(I, frequency, db))

    cp.onSolution {
      var actualSolKey = I.indices.filter(i => I(i).min == 1).mkString(" ")
      if (actualSolKey == "") actualSolKey = "<>"
      assert(output.contains(actualSolKey))
    }

    val statsActual = cp.start()

    assert(statsActual.nSols == output.size)

  }

}

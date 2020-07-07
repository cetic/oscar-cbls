package oscar.ml.pm.Constraints.fim

import oscar.cp.constraints.OrReif
import oscar.cp.testUtils.TestSuite
import oscar.cp.{CPBoolVar, CPIntVar, CPSolver, binaryStatic}
import oscar.ml.pm.utils.Dataset

import scala.io.Source

class CoverClosureTest extends TestSuite {
  test("CoverClosure vs Closed FIM with pure CP") {
    case class Config(
                       filename: String = "oscar-ml/src/main/scala/oscar/ml/pm/data/mushroom.txt",
                       minsup: Double = 0.40,
                       verbose: Boolean = false,
                       timeLimit: Int = 1000
                     )

    val config = Config()
    val db = Dataset(config.filename)
    val tdbHorizontal = db.getData()
    val tdbVertical = db.intoVertical()
    val nTrans = db.nbTrans
    val nItems = db.nbItem
    var frequency = config.minsup.intValue()

    if (config.minsup > 0 && config.minsup < 1) frequency = (config.minsup * nTrans).ceil.toInt //floor is another way around for the support

    //System.err.println(config + s"\nSup: $frequency\nnItems: $nItems\nnTrans: $nTrans")

    // Initializing the solver
    implicit val cp = CPSolver()

    // Declaring variables
    val I = Array.fill(nItems)(CPBoolVar()(cp))
    val T = Array.tabulate(nTrans)(t => CPBoolVar()(cp))
    val FreqVar = CPIntVar(frequency to nTrans)(cp)

    val Isorted = I.indices.sortBy(tdbVertical(_).size).map(I(_)).toArray

    cp.search(binaryStatic(Isorted))

    // Posting constraints
    val statsActual  = cp.startSubjectTo() {
      cp.add(new CoverClosure(I, FreqVar, db))
      cp.add(FreqVar >= frequency)
    }

    val statsExpected = cp.startSubjectTo() {
      /// Cover constraint
      for (t <- 0 until nTrans) {
        val tNotItems = (0 until nItems).filter(i => !tdbVertical(i).contains(t))
        cp.post(new OrReif(tNotItems.map(I(_)), !T(t)))
      }

      /// Closure constraint
      for (i <- 0 until nItems) {
        val tNotTrans = (0 until nTrans).filter(t => !tdbHorizontal(t).contains(i))
        cp.post(new OrReif(tNotTrans.map(T(_)), !I(i)))
      }

      /// Frequency
      cp.add(oscar.cp.modeling.constraint.sum(T) >=  FreqVar)
    }

    //println(statsActual)
    //println(statsExpected)

    assert(statsActual.nSols == statsExpected.nSols)
  }

  def readSols(filename: String): Map[String, Int] =
    Source.fromFile(filename).getLines
      .map(line => line.split(" #SUP: "))
      .map { case Array(itemset, support) => itemset -> support.toInt}
      .toMap

  test("Closed FIM check solution") {
    case class Config(
                       path: String = "oscar-ml/src/main/scala/oscar/ml/pm/data/test/",
                       filename: String = "input/contextPasquier99.txt",
                       solsFilename: String = "output/contextPasquier99-lcm-2.txt",
                       minsup: Double = 0.40,
                       verbose: Boolean = false,
                       timeLimit: Int = 1000
                     )

    val config = Config()
    val db = Dataset(config.path+config.filename)
    val tdbVertical = db.intoVertical()
    val nTrans = db.nbTrans
    val nItems = db.nbItem
    var frequency = config.minsup.intValue()
    val output = readSols(config.path+config.solsFilename)+("<>" -> nTrans)

    if (config.minsup > 0 && config.minsup < 1) frequency = (config.minsup * nTrans).ceil.toInt //floor is another way around for the support

    //System.err.println(config + s"\nSup: $frequency\nnItems: $nItems\nnTrans: $nTrans")

    // Initializing the solver
    implicit val cp = CPSolver()

    // Declaring variables
    val I = Array.fill(nItems)(CPBoolVar()(cp))
    val FreqVar = CPIntVar(0 to nTrans)(cp)

    val Isorted = I.indices.sortBy(tdbVertical(_).size).map(I(_)).toArray

    cp.search(binaryStatic(Isorted))

    // Posting constraints
    cp.add(new CoverClosure(I, FreqVar, db))
    cp.add(FreqVar >= frequency)

    cp.onSolution {
      var actualSolKey = I.indices.filter(i => I(i).min == 1).mkString(" ")
      if (actualSolKey == "") actualSolKey = "<>"
      assert(output(actualSolKey) == FreqVar.min)
    }

    val statsActual  = cp.start()

    assert(statsActual.nSols == output.size)

  }
}

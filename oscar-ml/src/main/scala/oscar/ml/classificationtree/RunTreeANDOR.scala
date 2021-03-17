package oscar.ml.classificationtree

import ANDORsearch.ANDORSearch
import DataManipulation.{Data, FileFormat, SparseFormat}
import DecisionTree.DTree
import oscar.cp.core.CPStore

/**
 * Code of the paper "Learning Optimal Decision Tree Using CP", H. Verhaeghe, S. Nijssen, C-G Quimpert, G. Pesant, P. Schaus
 * @author helene.verhaeghe27@gmail.com
 */
object RunTreeANDOR {

  def run(file: String,
          format: FileFormat,
          TO: Int,
          isComplete: Boolean,
          depth: Int,
          lbLeaf: Int,
          lbLeafPercent: Boolean,
          heuristic: Int,
          isPrunningMinActive: Boolean = true,
          isCacheActive: Boolean = true
         ): String = {

    val t = System.currentTimeMillis()
    implicit val solver = new CPStore()

    val db = Data(file, format)

    val tree = DTree(depth, solver, db)
    tree.countP.assign(db.nbTransP)
    tree.countM.assign(db.nbTransM)



    if (isComplete)
      tree.cstRemoveDecision(0)

    var threshold = lbLeaf
    if (lbLeafPercent)
      threshold = db.nbTrans * threshold / 100

    tree.cstCountSum
    tree.cstSumMini
    tree.cstLeftMostCountSum
    if (threshold > 0) {
      tree.cstLeafThreshold(threshold)
      tree.cstSplitPossible(threshold)
    }
    tree.cstDummy
    tree.cstCoverSize
    tree.cstAllDiffExcept0Path
    tree.cstSplitUseful
    tree.cstRemoveUseless(threshold)
    heuristic match {
      case 0 => tree.computeValueOrderingLexico
      case 1 => tree.computeValueOrderingEntropy
      case 2 => tree.computeValueOrdering
    }


    val t1 = System.currentTimeMillis()
    val (sol_tree, cost_tree) = if (isPrunningMinActive && isCacheActive) {
      ANDORSearch.search(solver, tree, TO)
    } else if (isCacheActive) {
      ANDORSearch.searchNoMin(solver, tree, TO)
    } else {
      ANDORSearch.searchNoCache(solver, tree, TO)
    }
    val t2 = System.currentTimeMillis()
    println("Total time: " + (t2 - t) )

    if(cost_tree < Int.MaxValue)
      sol_tree
    else
      "NOSOL"

  }


}

object solveDecisionTree extends App {


  val file ="./data/file.txt" // TODO CHANGE HERE
  val format: FileFormat = SparseFormat
  val TO: Int = 600
  val isComplete: Boolean = false
  val depth: Int = 4
  val lbLeaf: Int = 5
  val lbLeafPercent: Boolean = false
  val heuristic: Int = 1
  var isCacheActive = true
  var isPrunningMinActive = true
  println("===============")
  val treeANDOR = RunTreeANDOR.run(file, format, TO, isComplete, depth, lbLeaf, lbLeafPercent, heuristic,isPrunningMinActive,isCacheActive)
  println(treeANDOR)


}
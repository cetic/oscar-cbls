package oscar.cp.examples

import oscar.algo.search.VisualSearchTree
import oscar.cp._
import oscar.util.selectMin
import oscar.util.tree.Tree
import oscar.visual.VisualFrame

/**
 * Parcel Shipment Problem (found in the examples of Jacop).
 * A ship has to load or unload an amount at each city.
 * The objective is to find the shortest tour between the city
 * such that the current load remains under a given bound
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object ParcelShipment extends CPModel with App {

  // distance matrix between cities
  val distance: Array[Array[Int]] = Array(
    Array(0, 85, 110, 94, 71, 76, 25, 56, 94, 67),
    Array(85, 0, 26, 70, 62, 60, 63, 62, 70, 49),
    Array(110, 26, 0, 71, 87, 89, 88, 87, 93, 73),
    Array(94, 70, 71, 0, 121, 19, 82, 106, 124, 105),
    Array(71, 62, 87, 121, 0, 104, 53, 24, 8, 13),
    Array(76, 60, 89, 19, 104, 0, 65, 89, 108, 93),
    Array(25, 63, 88, 82, 53, 65, 0, 30, 57, 46),
    Array(56, 62, 87, 106, 24, 89, 30, 0, 23, 20),
    Array(94, 70, 93, 124, 8, 108, 57, 23, 0, 20),
    Array(67, 49, 73, 105, 13, 93, 46, 20, 20, 0))

  // Quantity to load in each city
  val toLoad = Array(0, 1, 5, -6, 4, 3, -5, 2, 1, -5)
  val n = distance.size
  val maxLoad = 7 // maximum load in the ship at any time
  val start = 0 // start city

  val succ = Array.tabulate(n)(c => CPIntVar(0 until n)) // successor
  val load = Array.tabulate(n)(c => CPIntVar(0 to maxLoad)) // load(i) is the load in the ship when leaving city i
  val totDist = CPIntVar(0 to distance.flatten.sum)
  val predStart = CPIntVar(0 until n)

  add(load(start) === 0) // start initially empty
  for (i <- 0 until n) {
    add(elementVar(load, succ(i), load(i) + toLoad(succ(i))))
  }
  add(sum(0 until n)(i => distance(i)(succ(i))) === totDist)
  add(circuit(succ), Strong)

  var currNode = 0
  val tree = new Tree()

  minimize(totDist) search {
    selectMin(succ)(!_.isBound)(_.size) match {
      case None => noAlternative
      case Some(x) => {
        val v = x.min
        val parent = currNode
        branch {
          post(x === v)
          currNode += 1
          val nodeId = currNode
          tree.createBranch(parent, currNode, currNode.toString, "left") {
            println("left in node" + nodeId)
          }
        } {
          post(x !== v)
          currNode += 1
          val nodeId = currNode
          tree.createBranch(parent, currNode, currNode.toString, "right") {
            println("right in node" + nodeId)
          }
        }
      }
    }

  }

  onSolution { tree.addSuccess(currNode) }

  val stats = solver.start()
  println(stats)

  val f = new VisualFrame("ParcelShipment", 1, 1)
  val w = f.createFrame("Tree")
  val vt = new VisualSearchTree(tree)
  w.add(vt)
  w.pack()

}

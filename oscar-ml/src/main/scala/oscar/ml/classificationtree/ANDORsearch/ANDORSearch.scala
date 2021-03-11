/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.ml.classificationtree.ANDORsearch

import oscar.ml.classificationtree.DecisionTree.{DEndNode, DNode, DTreeDecision}
import oscar.algo.Inconsistency
import oscar.cp.core.CPStore
/**
 * Code of the paper "Learning Optimal Decision Tree Using CP", H. Verhaeghe, S. Nijssen, C-G Quimpert, G. Pesant, P. Schaus
 * @author helene.verhaeghe27@gmail.com
 */
object ANDORSearch {

  def search(solver: CPStore, problem: DTreeDecision, TO: Int = -1): (String, Int) = {
    println("search start")
    val initTime = System.currentTimeMillis()
    val maxTime = (TO * 1000) + initTime
    val stopcondition = if (TO > 0) () => System.currentTimeMillis() >= maxTime else () => false
    val storage = problem.getNewStorageANDOR


    def ORnode(problem: DTreeDecision, cost_ub: Int): (String, Int) = {
      var cost_best = cost_ub
      var sol_best = ""
      val hash = problem.getHash
      if (storage.contains(hash)) {
        storage(hash)
      } else {
        if (!stopcondition()) {
          if (problem.decision.isBound) {
            val (sol_tree, cost_tree) =
              problem match {
                case p: DNode =>
                  ANDnode(p, cost_best, problem.decision.min - 1)
                case p: DEndNode =>
                  (problem.decision.min - 1 + "(" + problem.leftChild.leafString + "," + problem.rightChild.leafString + ")", p.miniSum.min)
              }
            if (cost_best > cost_tree) {
              cost_best = cost_tree
              if (problem.decision.min == 0)
                sol_best = problem.leafString
              else
                sol_best = sol_tree
              storage += ((hash, (sol_best, cost_best)))
            }
          } else {
            solver.pushState()
            try {
              //              solver.smaller(problem.miniSum, cost_best)
              solver.assign(problem.decision, 0)
              val (sol_tree, cost_tree) =
                problem match {
                  case p: DNode =>
                    ANDnode(p, cost_best, 0)
                  case p: DEndNode =>
                    (0 + "(" + problem.leftChild.leafString + "," + problem.rightChild.leafString + ")", p.miniSum.min)
                }
              if (cost_best > cost_tree) {
                cost_best = cost_tree
                sol_best = problem.leafString
                storage += ((hash, (sol_best, cost_best)))
              }
            } catch {
              case e: Inconsistency =>
            }
            solver.pop()
            if (!stopcondition()) {
              solver.pushState()
              try {
                solver.remove(problem.decision, 0)
                for (f <- problem.valueOrdering) {
                  if (!stopcondition()) {
                    solver.pushState()
                    try {
                      solver.smaller(problem.miniSum, cost_best)
                      solver.assign(problem.decision, f)
                      val (sol_tree, cost_tree) =
                        problem match {
                          case p: DNode =>
                            ANDnode(p, cost_best, f - 1)
                          case p: DEndNode =>
                            (f - 1 + "(" + problem.leftChild.leafString + "," + problem.rightChild.leafString + ")", p.miniSum.min)
                        }
                      if (cost_best > cost_tree) {
                        cost_best = cost_tree
                        sol_best = sol_tree
                        storage += ((hash, (sol_best, cost_best)))
                      }
                    } catch {
                      case e: Inconsistency =>
                    }
                    solver.pop()
                  }
                }
              } catch {
                case e: Inconsistency =>
              }
              solver.pop()
            }
          }

        }
        (sol_best, cost_best)
      }
    }

    def ANDnode(problem: DNode, cost_ub: Int, f_root: Int): (String, Int) = {
      val (sol_left, cost_left) = ORnode(problem.leftChild, cost_ub)
      if (cost_left > cost_ub) {
        return ("", Int.MaxValue)
      }
      val (sol_right, cost_right) = ORnode(problem.rightChild, cost_ub - cost_left)
      val sol_tree = f_root + "(" + sol_left + "," + sol_right + ")"
      (sol_tree, cost_left + cost_right)

    }


    val sol = ORnode(problem, Int.MaxValue)
    val endtime = System.currentTimeMillis()
    val end = !stopcondition()

    println("storage size: " + storage.size)
    val hash = problem.getHash
    println("hash initit node: " + hash)
    if (storage.contains(hash)) {
      println("Solution: " + storage(hash)._1)
      println("Solution (should be equal to other one): " + sol._1)
      println("Cost: " + storage(hash)._2)
    } else {
      println("Solution: NOSOL")
      println("Cost: " + Int.MaxValue)
    }
    println("Search time: " + (endtime - initTime))
    println("Completed: " + end)
    sol
  }

  def searchNoMin(solver: CPStore, problem: DTreeDecision, TO: Int = -1): (String, Int) = {
    val initTime = System.currentTimeMillis()
    val maxTime = (TO * 1000) + initTime
    val stopcondition = if (TO > 0) () => System.currentTimeMillis() >= maxTime else () => false
    val storage = problem.getNewStorageANDOR


    def ORnode(problem: DTreeDecision, cost_ub: Int): (String, Int) = {
      var cost_best = cost_ub
      var sol_best = ""
      val hash = problem.getHash
      if (storage.contains(hash)) {
        storage(hash)
      } else {
        if (!stopcondition()) {
          if (problem.decision.isBound) {
            val (sol_tree, cost_tree) =
              problem match {
                case p: DNode =>
                  ANDnode(p, cost_best, problem.decision.min - 1)
                case p: DEndNode =>
                  (problem.decision.min - 1 + "(" + problem.leftChild.leafString + "," + problem.rightChild.leafString + ")", p.miniSum.min)
              }
            if (cost_best > cost_tree) {
              cost_best = cost_tree
              if (problem.decision.min == 0)
                sol_best = problem.leafString
              else
                sol_best = sol_tree
              storage += ((hash, (sol_best, cost_best)))
            }
          } else {
            solver.pushState()
            try {
              solver.assign(problem.decision, 0)
              val (sol_tree, cost_tree) =
                problem match {
                  case p: DNode =>
                    ANDnode(p, cost_best, 0)
                  case p: DEndNode =>
                    (0 + "(" + problem.leftChild.leafString + "," + problem.rightChild.leafString + ")", p.miniSum.min)
                }
              if (cost_best > cost_tree) {
                cost_best = cost_tree
                sol_best = problem.leafString
                storage += ((hash, (sol_best, cost_best)))
              }
            } catch {
              case e: Inconsistency =>
            }
            solver.pop()
            if (!stopcondition()) {
              solver.pushState()
              try {
                solver.remove(problem.decision, 0)
                for (f <- problem.valueOrdering) {
                  if (!stopcondition()) {
                    solver.pushState()
                    try {
                      //                      solver.smaller(problem.miniSum, cost_best)
                      solver.assign(problem.decision, f)
                      val (sol_tree, cost_tree) =
                        problem match {
                          case p: DNode =>
                            ANDnode(p, cost_best, f - 1)
                          case p: DEndNode =>
                            (f - 1 + "(" + problem.leftChild.leafString + "," + problem.rightChild.leafString + ")", p.miniSum.min)
                        }
                      if (cost_best > cost_tree) {
                        cost_best = cost_tree
                        sol_best = sol_tree
                        storage += ((hash, (sol_best, cost_best)))
                      }
                    } catch {
                      case e: Inconsistency =>
                    }
                    solver.pop()
                  }
                }
              } catch {
                case e: Inconsistency =>
              }
              solver.pop()
            }
          }

        }
        (sol_best, cost_best)
      }
    }

    def ANDnode(problem: DNode, cost_ub: Int, f_root: Int): (String, Int) = {
      val (sol_left, cost_left) = ORnode(problem.leftChild, cost_ub)
      if (cost_left > cost_ub) {
        return ("", Int.MaxValue)
      }
      val (sol_right, cost_right) = ORnode(problem.rightChild, cost_ub - cost_left)
      val sol_tree = f_root + "(" + sol_left + "," + sol_right + ")"
      (sol_tree, cost_left + cost_right)

    }


    val sol = ORnode(problem, Int.MaxValue)
    val endtime = System.currentTimeMillis()
    val end = !stopcondition()

    println("storage size: " + storage.size)
    val hash = problem.getHash
    println("hash initit node: " + hash)
    if (storage.contains(hash)) {
      println("Solution: " + storage(hash)._1)
      println("Solution (should be equal to other one): " + sol._1)
      println("Cost: " + storage(hash)._2)
    } else {
      println("Solution: NOSOL")
      println("Cost: " + Int.MaxValue)
    }
    println("Search time: " + (endtime - initTime))
    println("Completed: " + end)
    sol
  }


  def searchNoCache(solver: CPStore, problem: DTreeDecision, TO: Int = -1): (String, Int) = {
    val initTime = System.currentTimeMillis()
    val maxTime = (TO * 1000) + initTime
    val stopcondition = if (TO > 0) () => System.currentTimeMillis() >= maxTime else () => false


    def ORnode(problem: DTreeDecision, cost_ub: Int): (String, Int) = {
      var cost_best = cost_ub
      var sol_best = ""

      if (!stopcondition()) {
        if (problem.decision.isBound) {
          val (sol_tree, cost_tree) =
            problem match {
              case p: DNode =>
                ANDnode(p, cost_best, problem.decision.min - 1)
              case p: DEndNode =>
                (problem.decision.min - 1 + "(" + problem.leftChild.leafString + "," + problem.rightChild.leafString + ")", p.miniSum.min)
            }
          if (cost_best > cost_tree) {
            cost_best = cost_tree
            if (problem.decision.min == 0)
              sol_best = problem.leafString
            else
              sol_best = sol_tree
          }
        } else {
          solver.pushState()
          try {
            //            solver.smaller(problem.miniSum, cost_best)
            solver.assign(problem.decision, 0)
            val (sol_tree, cost_tree) =
              problem match {
                case p: DNode =>
                  ANDnode(p, cost_best, 0)
                case p: DEndNode =>
                  (0 + "(" + problem.leftChild.leafString + "," + problem.rightChild.leafString + ")", p.miniSum.min)
              }
            if (cost_best > cost_tree) {
              cost_best = cost_tree
              sol_best = problem.leafString
            }
          } catch {
            case e: Inconsistency =>
          }
          solver.pop()
          if (!stopcondition()) {
            solver.pushState()
            try {
              solver.remove(problem.decision, 0)
              for (f <- problem.valueOrdering) {
                if (!stopcondition()) {
                  solver.pushState()
                  try {
                    solver.smaller(problem.miniSum, cost_best)
                    solver.assign(problem.decision, f)
                    val (sol_tree, cost_tree) =
                      problem match {
                        case p: DNode =>
                          ANDnode(p, cost_best, f - 1)
                        case p: DEndNode =>
                          (f - 1 + "(" + problem.leftChild.leafString + "," + problem.rightChild.leafString + ")", p.miniSum.min)
                      }
                    if (cost_best > cost_tree) {
                      cost_best = cost_tree
                      sol_best = sol_tree
                    }
                  } catch {
                    case e: Inconsistency =>
                  }
                  solver.pop()
                }
              }
            } catch {
              case e: Inconsistency =>
            }
            solver.pop()
          }
        }

      }
      (sol_best, cost_best)

    }

    def ANDnode(problem: DNode, cost_ub: Int, f_root: Int): (String, Int) = {
      val (sol_left, cost_left) = ORnode(problem.leftChild, cost_ub)
      if (cost_left > cost_ub) {
        return ("", Int.MaxValue)
      }
      val (sol_right, cost_right) = ORnode(problem.rightChild, cost_ub - cost_left)
      val sol_tree = f_root + "(" + sol_left + "," + sol_right + ")"
      (sol_tree, cost_left + cost_right)

    }


    val sol = ORnode(problem, Int.MaxValue)
    val endtime = System.currentTimeMillis()
    val end = !stopcondition()


    if (sol._2 < Int.MaxValue) {
      println("Solution: " + sol._1)
      println("Cost: " + sol._2)
    } else {
      println("Solution: NOSOL")
      println("Cost: " + Int.MaxValue)
    }
    println("Search time: " + (endtime - initTime))
    println("Completed: " + end)
    sol
  }
}

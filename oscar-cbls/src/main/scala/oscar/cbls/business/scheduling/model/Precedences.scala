package oscar.cbls.business.scheduling.model

import oscar.cbls.business.scheduling.ActivityId

import scala.annotation.tailrec
import scala.collection.BitSet

// Precedences
/**
  * This class is a container for the precedence constraints between activity indices
  */
class Precedences(beforeAfterPairs: List [(ActivityId, ActivityId)]) {
  // Predecessors map
  var predMap: Map[ActivityId, BitSet] = Map()
  // Successors map
  var succMap: Map[ActivityId, BitSet] = Map()
  // Filling maps from precedences pairs
  for {(actA, actB) <- beforeAfterPairs} {
    val predB = predMap.getOrElse(actB, BitSet.empty)
    val succA = succMap.getOrElse(actA, BitSet.empty)
    predMap += (actB -> (predB + actA))
    succMap += (actA -> (succA + actB))
  }

  /**
    * Gets a priority list according to the precedences constraints
    *
    * @return a list containing a permutation of [0..numActivity) corresponding
    *         to a consistent priority list (if A->B, index of A is before index of B in the list)
    */
  def getPriorityList(activitiesOnList: Iterable[ActivityId]): List[Int] = {
    def insertList(i: ActivityId, succI: BitSet, accList: List[Int]): List[Int] = {
      accList match {
        case Nil => List(i)
        case x::xs =>
          if (succI.contains(x.toInt))
            i::accList
          else
            x::insertList(i, succI, xs)
      }
    }
    /////
    var result: List[Int] = Nil
    for {i <- activitiesOnList} {
      result = insertList(i, succMap.getOrElse(i, BitSet.empty), result)
    }
    result
  }

  /**
    * Checks whether a list of activity indexes is consistent with the precedences
    *
    * @param seq a sequence of activities
    * @return true iff all indices in seq respect the precedences relation
    */
  def consistentSeq(seq: List[ActivityId]): Boolean = {
    // Auxiliary function
    @tailrec
    def consistentSeq(postfix: List[ActivityId],
                      revPrefix: List[ActivityId]): Boolean = postfix match {
      case Nil => true
      case act::acts =>
        val notPrecPref = !revPrefix.exists(descendants(act).contains(_))
        if (notPrecPref)
          consistentSeq(acts, act::revPrefix)
        else
          false
    }
    /////
    consistentSeq(seq, Nil)
  }

  /**
    * Get a s list from the precedence relation
    *
    * @return a list of pairs (a, b) where a->b according to the precedence relation
    */
  def toPairsList: List[(ActivityId, ActivityId)] = {
    var pairsList: List[(ActivityId, ActivityId)] = Nil
    for {i <- predMap.keys} {
      val indPairs = predMap(i).map((i,_)).toList
      pairsList :::= indPairs
    }
    pairsList
  }

  /**
    * Determine the "descendants" of an activity in the precedence constraints
    *
    * @param act the index of the activity
    * @return a list of the indices for activities in the transitive closure of
    *         the precedence relation
    */
  private def descendants(act: ActivityId): List[ActivityId] = {
    // Auxiliary function
    @tailrec
    def descendants(lstActs: List[ActivityId],
                    visitedActs: List[ActivityId]): List[ActivityId] = lstActs match {
      case Nil => visitedActs
      case act::acts =>
        if (visitedActs.contains(act)) {
          descendants(acts, visitedActs)
        }
        else {
          descendants(succMap.getOrElse(act, Set()).toList:::acts, act::visitedActs)
        }
    }
    /////
    descendants(List(act), Nil)
  }

  /**
    * To string
    *
    * @return a readable string for this precedence relation
    */
  override def toString: String = {
    val strPrec = predMap.foldLeft("[")((str, s) => s"$str{$s} ")
    val strSucc = succMap.foldLeft("[")((str, s) => s"$str{$s} ")
    s"Precedences:\n** Direct Precedences : $strPrec\n** Direct Successors : $strSucc"
  }
}

object Precedences {
  def apply(beforeAfterPairs: List[(ActivityId, ActivityId)]): Precedences =
    new Precedences(beforeAfterPairs)
}

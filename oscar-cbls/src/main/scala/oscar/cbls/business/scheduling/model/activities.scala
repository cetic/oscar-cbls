package oscar.cbls.business.scheduling.model

import oscar.cbls.business.scheduling.Activity

import scala.collection.BitSet

abstract class ActivityType
case object Mandatory extends ActivityType
case object Flexible extends ActivityType
case object Optional extends ActivityType

case class ActivityData(activity: Activity,
                        duration: Long,
                        minStartTime: Long,
                        `type`: ActivityType)

// Precedences
/**
  * This class is a container for the precedence constraints between activity indices
  */
class PrecedencesData(beforeAfterPairs: List [(Activity, Activity)]) {
  // Predecessors map
  var predMap: Map[Activity, BitSet] = Map()
  // Successors map
  var succMap: Map[Activity, BitSet] = Map()
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
  def getPriorityList(activitiesOnList: Iterable[Activity]): List[Long] = {
    def insertList(i: Activity, succI: BitSet, accList: List[Long]): List[Long] = {
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
    var result: List[Long] = Nil
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
  def consistentSeq(seq: List[Activity]): Boolean = {
    // Auxiliary function
    def consistentSeq(postfix: List[Activity],
                      revPrefix: List[Activity]): Boolean = postfix match {
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
  def toPairsList: List[(Activity, Activity)] = {
  var pairsList: List[(Activity, Activity)] = Nil
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
  private def descendants(act: Activity): List[Activity] = {
    // Auxiliary function
    def descendants(lstActs: List[Activity],
                    visitedActs: List[Activity]): List[Activity] = lstActs match {
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

object PrecedencesData {
  def apply(beforeAfterPairs: List[(Activity, Activity)]): PrecedencesData =
    new PrecedencesData(beforeAfterPairs)
}

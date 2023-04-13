package oscar.cbls.business.scheduling.model

import oscar.cbls.business.scheduling.ActivityId

import scala.annotation.tailrec
import scala.collection.BitSet

// Precedences
/**
  * This class is a container for the precedence constraints between activity indices
  */
class Precedences(activities: List[ActivityId],
                  beforeAfterPairs: List [(ActivityId, ActivityId)]) {
  // Predecessors map
  var predMap: Map[ActivityId, BitSet] = activities.map(_ -> BitSet.empty).toMap
  // Successors map
  var succMap: Map[ActivityId, BitSet] = activities.map(_ -> BitSet.empty).toMap
  // Filling maps from precedences pairs
  for {(actA, actB) <- beforeAfterPairs} {
    val predB = predMap.getOrElse(actB, BitSet.empty)
    val succA = succMap.getOrElse(actA, BitSet.empty)
    predMap += (actB -> (predB union Set(actA)))
    succMap += (actA -> (succA union Set(actB)))
  }
  val (ancestorsMap, descendantsMap) = ancestorsAndDescendants

  def ancestorsAndDescendants: (Map[ActivityId, BitSet], Map[ActivityId, BitSet]) = {
    // Ancestors function
    def ancestors(act: ActivityId): BitSet = {
      // Auxiliary function
      @tailrec
      def ancestors(act: ActivityId,
                    toExploreActs: List[ActivityId],
                    visitedActs: BitSet): BitSet = toExploreActs match {
        case Nil => visitedActs
        case a::acts =>
          if ((a == act) || visitedActs.contains(a))
            ancestors(act, acts, visitedActs)
          else
            ancestors(act, predMap.getOrElse(a, Set()).toList:::acts, visitedActs union Set(a))
      }
      /////
      ancestors(act, predMap.getOrElse(act, Set()).toList, BitSet.empty)
    }
    // Descendants function
    def descendants(act: ActivityId): BitSet = {
      // Auxiliary function
      @tailrec
      def descendants(act: ActivityId,
                      toExploreActs: List[ActivityId],
                      visitedActs: BitSet): BitSet = toExploreActs match {
        case Nil => visitedActs
        case a::acts =>
          if ((a == act) || visitedActs.contains(a))
            descendants(act, acts, visitedActs)
          else
            descendants(act, succMap.getOrElse(a, Set()).toList:::acts, visitedActs union Set(a))
      }
      /////
      descendants(act, succMap.getOrElse(act, Set()).toList, BitSet.empty)
    }
    //////////
    // Filling ancestors and descendants map
    var ancestorsMap: Map[ActivityId, BitSet] = Map()
    var descendantsMap: Map[ActivityId, BitSet] = Map()
    activities.foreach { act =>
      ancestorsMap += (act -> ancestors(act))
      descendantsMap += (act -> descendants(act))
    }
    (ancestorsMap, descendantsMap)
  }

  /**
    * Gets a priority list according to the precedences constraints
    *
    * @return a list containing a permutation of [0..numActivity) corresponding
    *         to a consistent priority list (if A->B, index of A is before index of B in the list)
    */
  def getPriorityList(activitiesOnList: Iterable[ActivityId]): List[ActivityId] = {
    def insertList(i: ActivityId,
                   accList: List[ActivityId]): List[ActivityId] = {
      accList match {
        case Nil => List(i)
        case x::xs =>
          if (descendantsMap(i).contains(x))
            i::accList
          else
            x::insertList(i, xs)
      }
    }
    /////
    var result: List[Int] = Nil
    for {i <- activitiesOnList} {
      result = insertList(i, result)
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
                      revPrefix: List[ActivityId],
                      precPref: Boolean): Boolean = {
      if (precPref)
        false
      else {
        postfix match {
          case Nil => true
          case act::acts =>
            consistentSeq(acts, act::revPrefix, revPrefix.exists(descendantsMap(act).contains))
        }
      }
    }
    /////
    consistentSeq(seq, Nil, precPref = false)
  }

  def inconsistenceMap(seq: List[ActivityId]): Map[ActivityId, List[ActivityId]] = {
    @tailrec
    def inconsistenceMap(ls: List[ActivityId],
                         prefAcc: List[ActivityId],
                         mapAcc: Map[ActivityId, List[ActivityId]]): Map[ActivityId, List[ActivityId]] = {
      ls match {
        case Nil => mapAcc
        case a::as =>
          val inconsistences = prefAcc.filter(descendantsMap(a).contains)
          inconsistenceMap(as, a::prefAcc, mapAcc + (a -> inconsistences))
      }
    }
    //////////
    inconsistenceMap(seq, Nil, Map())
  }

  /**
    * Get a s list from the precedence relation
    *
    * @return a list of pairs (a, b) where a->b according to the precedence relation
    */
  def toPairsList: List[(ActivityId, ActivityId)] = {
    var pairsList: List[(ActivityId, ActivityId)] = Nil
    for {i <- ancestorsMap.keys} {
      val indPairs = ancestorsMap(i).map((_,i)).toList
      pairsList :::= indPairs
    }
    pairsList
  }

  /**
    * To string
    *
    * @return a readable string for this precedence relation
    */
  override def toString: String = {
    val strPrec = ancestorsMap.foldLeft("[")((str, s) => s"$str{$s} ")
    val strSucc = descendantsMap.foldLeft("[")((str, s) => s"$str{$s} ")
    s"Precedences:\n** Ancestors : $strPrec\n** Descendants : $strSucc"
  }
}

object Precedences {
  def apply(activities: List[ActivityId],
            beforeAfterPairs: List[(ActivityId, ActivityId)]): Precedences =
    new Precedences(activities, beforeAfterPairs)
}

package oscar.cbls.test.scheduling

import oscar.cbls._
import oscar.cbls.business.scheduling.model._
import oscar.cbls.business.scheduling.neighborhood._
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}
import oscar.cbls.lib.invariant.seq.SeqSum

import scala.util.Random

object MinStartTimesExample extends App {
  // Random Generator
  val randomGen = new Random(10)

  val s = Store(checker = Some(new ErrorChecker()))

  //données
  val nAct = 10 //nbre d'activités
  val minDuration = 10  //nombre minimum de minutes pour une activité
  val maxDuration = 30  //nombre maximum de minutes pour une activité
  val penaltyForUnscheduled = 10000L  //penalité d'une tâche pour ne pas être dans le schedule

  //créations des variables pour le modèle
  val activities = (0 until nAct).toList

  //1) durations(i) = durée de l'activité i
  val durations = activities.map { i =>
    (i, randomInterval(minDuration, maxDuration))
  }.toMap

  val minStartTimes = Map(0 -> 30)

  println("Activities:")
  activities.foreach(act => println(s"Activity $act"))

  //2) les contraintes de précédences (on peut mettre une liste vide)
  val precPairs = List((0, 3), (2, 1)) //(a,b) l'activité numéro 0 doit se dérouler avant l'activité numéro 3

  //3) Création des resources

  val resource1 = new DisjunctiveResource(List(0,2,5,7,8,9))
  val resource2 = new CumulativeResource(5L,Map(1-> 2L , 3 -> 1L , 4 -> 1L , 6 -> 1L , 7 -> 1L))

  val resources = List(resource1,resource2)

  //la contrainte de scheduling
  val scheduling = new Schedule(s, activities, Nil, durations, minStartTimes, precPairs, resources)

  // Pred and succ maps
  scheduling.precedencesData.predMap.foreach(pi => println(s"Pred ${pi._1} => ${pi._2}"))
  scheduling.precedencesData.succMap.foreach(si => println(s"Succ ${si._1} => ${si._2}"))

  val sumScheduled = SeqSum(scheduling.activityPriorityList, i => { nAct - i }) //, i => func(i.toInt))

  //la fonction objective
  val objFunc = Objective(scheduling.makeSpan + (penaltyForUnscheduled * (nAct - length(scheduling.activityPriorityList))))
  //val objFunc = Objective(scheduling.makeSpan)

  //fermer le modèle
  s.close()

  //les voisinages d'exploration
  /*
   * Liste des voisinages
   * - AddActivity
   * - ReinsertActivity
   * - RemoveActivity
   * - ReplaceActivity
   * - SchedulingMove
   * - SwapActivity
   */

  val swapNH = new SwapActivity(scheduling, "Swap")
  val reinsertNH = new ReinsertActivity(scheduling, "Reinsert")
  val addNH = new AddActivity(scheduling, "Add")
  val removeNH = new RemoveActivity(scheduling, "Remove")
  //val replaceNHcomb = removeNH dynAndThen (_ => addNH)
  val replaceNH = new ReplaceActivity(scheduling, "Replace")
  //val combinedNH = Profile(replaceNHcomb)
  val combinedNH = BestSlopeFirst(List(Profile(addNH), Profile(reinsertNH), Profile(swapNH), Profile(replaceNH)))
  //Profile c'est pour les stats : à enlever

  //lancement de la recherche
  combinedNH.verbose = 1
  combinedNH.doAllMoves(obj = objFunc)

  println(combinedNH.profilingStatistics)
  println(s"Activities = ${scheduling.activityPriorityList.value.toList}")

  println("------------------------")

  println("Scheduling start times = [  ")
  scheduling.startTimes.foreach(v => println(s"    $v"))
  println("]")

  println(s"Objective Function = $objFunc")

  /**
   * Rend un nombre aléatoire entre inf et sup
   */

  def randomInterval(inf: Int, sup: Int): Int = {
    require(inf <= sup)
    val rdVal = if (inf == sup) 0 else Math.abs(randomGen.nextInt()) % (sup - inf)
    inf + rdVal
  }
}
/*package oscar.cbls.core.search
import oscar.cbls.core.objective.Objective

import scala.collection.SortedMap

class RemoteCombinator(remoteChildren:Array[Neighborhood])
  extends NeighborhoodCombinator(remoteChildren:_*) {

  def labelRemoteChildren(currentRemoteID:Int):SortedMap[Int,Neighborhood] = {

  }
  override def getMove(obj: Objective, initialObj: Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = ???
}


class RemoteStaticNeighborhood(n:Neighborhood) extends NeighborhoodCombinator(n){


  override def getMove(obj: Objective, initialObj: Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = ???

  /**
   * problème: il faut passer
   * obj => distributedObj class, easy to remap! each obj has a unique ID, obj can be remappedat distance.
   *    the unique ID are defined at model close and only these obj can be remapped
   *    not visible to the user.
   * acceptance => use serializable class here; need to modify oscaR for that purpose.
   *              also, simulated annealing calls for special acceptance criterion.
   *              there will be a problem with temperature management. maybe not so much.
   * currentSolution => model.save
   *
   * how about making the opportunity to change acceptance criterion and obj to something more robust?
   * that encompasses the cascading obj?
   *
   * we already have an index on all variables, how about just using this index for obj identification?
   *
   * how to transmit moves over the network?
   * serialization/deserialization of case classes?
   * special serialization method?
   * variables being serialized by only dumping their ID and type,
   * so moves can be serialized naively
   */


  def startLocalComputation(obj: Objective,
                            initialObj: Long,
                            acceptanceCriterion: (Long, Long) => Boolean): Unit ={

  }

  def startRemoteComputationSyncronized(obj: Objective,
                                        initialObj: Long,
                                        acceptanceCriterion: (Long, Long) => Boolean): SearchResult ={

    //sauver le modèle pour l'envoyer
    //concertir (modèle, obj, acceptance) en process_independent)
    //envoyer (modèle, obj, initObj, acceptance)
    //attendre le résultat
    //convertir le résultat en

  }
}


/**
 * obj et acc peuvent évoluer:
 *
 * goal = obj+acc
 *
 * permet
 * *contrainte forte (cascading)
 * *objectifs lexicographoques
 * *acceptance criterion
 * *modifier l'ordre d'évaluation entre acceptance et contraintes fortes (oui, mais ça va être difficile si on veut pouvoir changer le critère d'acceptation)
 *
 * on peut envisager deux méthodes:
 *
 * obj.isCurrentNeighborAcceptable(move:Move):Boolean pour tester si la situation actuelle est acceptable (pour les best/first)
 *
 *
 */


/**
 * comment identifier un fils remote?
 * Un fils remote peut aller s'enregistrer automatiquemmnt dans une objet de référence
 * on peut parcourir l'arbre statique pour identifier les fils remote statquement instanciés (préféré)
 *
 */


*/
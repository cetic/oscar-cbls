package com.example.cbls

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}

import scala.concurrent.{Future, Promise}

sealed trait MessageToWorkGiver

abstract class SearchEnded(val searchID:Long) extends MessageToWorkGiver
final case  class SearchCompleted(override val searchID:Long, searchResult:IndependentSearchResult) extends SearchEnded(searchID)
final case  class SearchAborted(override val searchID:Long) extends SearchEnded(searchID)
final case class SearchCrashed(override val searchID:Long, neighborhood:RemoteNeighborhoodIdentification, exception:Throwable, worker:ActorRef[MessageToWorker]) extends SearchEnded(searchID)

final case class CancelSearch() extends MessageToWorkGiver
final case class PromiseResult(replyTo:ActorRef[Future[SearchEnded]]) extends MessageToWorkGiver

object ORWorkGiver{
 def apply (supervisorActorRef:ActorRef[MessagesToSupervisor],
            searchIDs:Array[Long],
            verbose:Boolean = false):Behavior[MessageToWorkGiver] =
  Behaviors.setup[MessageToWorkGiver](context => new ORWorkGiverActor(supervisorActorRef, searchIDs, context, verbose))
}

class ORWorkGiverActor(supervisor:ActorRef[MessagesToSupervisor],
                       searchIDs:Array[Long],
                       context:ActorContext[MessageToWorkGiver],
                       verbose:Boolean)
  extends AbstractBehavior(context:ActorContext[MessageToWorkGiver]) {

 private val resultPromize = Promise[SearchEnded]()
 private val futureForResult:Future[SearchEnded] = resultPromize.future

 if(verbose) context.log.info(s"created for searches:${searchIDs(0)}_${searchIDs(searchIDs.length-1)}")

 private var remainingSearches = searchIDs.length

 def cancelSearches(): Unit ={
  if(verbose) context.log.info(s"cancelling searches:${searchIDs.mkString(",")}")
  for(searchID <- searchIDs){
   supervisor!CancelSearchToSupervisor(searchID)
  }
 }

 override def onMessage(msg: MessageToWorkGiver): Behavior[MessageToWorkGiver] = {
  msg match {
   case PromiseResult(replyTo: ActorRef[Future[SearchEnded]]) =>
    replyTo ! futureForResult

   case c: SearchEnded =>
    c match {
     case SearchCompleted(_, m: IndependentMoveFound) =>
      if(verbose) context.log.info(s"got result for search:${c.searchID} : $c")
      resultPromize.success(c)
      cancelSearches()

     case SearchCrashed(searchID:Long, neighborhood, exception:Throwable, worker) =>
      //in this case, we avoid silent error, an report eh crash.
      if(verbose) context.log.info(s"got crash report at worker ${worker} for search:${c.searchID}")
      resultPromize.success(c)

     case _ =>
      remainingSearches -= 1
      if(remainingSearches == 0) {
       resultPromize.success(new SearchCompleted(-1,IndependentNoMoveFound()))
       cancelSearches()
      }
    }

   case CancelSearch() =>
    if(verbose) context.log.info(s"received cancel search command for searches:${searchIDs.mkString(",")}; forwarding to Supervisor")
    cancelSearches()
  }

  this
 }
}


object WorkGiver{
 def apply(supervisorActorRef:ActorRef[MessagesToSupervisor],
           searchID:Long,
           forwardResultOpt:Option[ActorRef[MessageToWorkGiver]] = None,
           verbose:Boolean = false):Behavior[MessageToWorkGiver] =
  Behaviors.setup[MessageToWorkGiver](context => new WorkGiverActor(supervisorActorRef, searchID, context, forwardResultOpt, verbose))
}

class WorkGiverActor(supervisor:ActorRef[MessagesToSupervisor],
                     searchID:Long,
                     context:ActorContext[MessageToWorkGiver],
                     forwardResultOpt:Option[ActorRef[MessageToWorkGiver]] = None,
                     verbose:Boolean)
  extends AbstractBehavior(context:ActorContext[MessageToWorkGiver]) {

 private val resultPromize = Promise[SearchEnded]()
 private val futureForResult:Future[SearchEnded] = resultPromize.future

 if(verbose) context.log.info(s"created for search:$searchID")
 override def onMessage(msg: MessageToWorkGiver): Behavior[MessageToWorkGiver] = {
  msg match {
   case PromiseResult(replyTo: ActorRef[Future[SearchEnded]]) =>
    replyTo ! futureForResult

   case c: SearchEnded =>
    require(c.searchID == searchID)
    if (c.searchID == searchID) {
     if(verbose) context.log.info(s"got result for search:$searchID : $c")
     resultPromize.success(c)
     forwardResultOpt match{
      case Some(t) => t!c
      case None => ;
     }
    } else {
     //received success about another search?!
     if(verbose) context.log.error(s"got result for another search:${c.searchID}, was expecting $searchID; ignoring")
    }

    c match {
     case SearchCrashed(searchID: Long, neighborhood, exception:Throwable, worker) =>
      //in this case, we avoid silent error, an report eh crash.
      if(verbose) context.log.info(s"got crash report at worker ${worker} for search:${c.searchID}")
     case _ => ;
    }

   case CancelSearch() =>
    if(verbose) context.log.info(s"received cancel search command for search:$searchID; forwarding to Supervisor")
    supervisor ! CancelSearchToSupervisor(searchID)
  }
  this
 }
}


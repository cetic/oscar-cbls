package oscar.cbls.core.distrib

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import oscar.cbls.core.search.SearchResult

import scala.concurrent.{Future, Promise}
import scala.util.Success

sealed trait MessageToWorkGiver

abstract sealed class SearchEnded(val searchID:Long) extends MessageToWorkGiver
final case  class SearchCompleted(override val searchID:Long, searchResult:IndependentSearchResult) extends SearchEnded(searchID)
final case  class SearchAborted(override val searchID:Long) extends SearchEnded(searchID)
final case class SearchCrashed(override val searchID:Long, neighborhood:RemoteNeighborhoodIdentification, exception:Throwable, worker:ActorRef[MessageToWorker]) extends SearchEnded(searchID)

final case class CancelSearch() extends MessageToWorkGiver
final case class PromiseResult(replyTo:ActorRef[Future[SearchEnded]]) extends MessageToWorkGiver

object ORWorkGiverActor{
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

 private val resultPromise = Promise[SearchEnded]()
 private val futureForResult:Future[SearchEnded] = resultPromise.future

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
     case SearchCompleted(_, searchResult) =>
      searchResult match{
       case m:IndependentMoveFound =>
        if(verbose) context.log.info(s"got result for search:${c.searchID} : $c")
        if(!resultPromise.isCompleted) {
         resultPromise.success(c)
         cancelSearches()
        }
       case _:IndependentNoMoveFound =>
        remainingSearches -= 1
        if(remainingSearches == 0) {
         if(!resultPromise.isCompleted) {
          resultPromise.success(SearchCompleted(-1,IndependentNoMoveFound()))
         }
        }
      }

     case SearchCrashed(searchID:Long, neighborhood, exception:Throwable, worker) =>
      //in this case, we avoid silent error, an report eh crash.
      if(verbose) context.log.info(s"got crash report at worker $worker for search:${c.searchID}")
      if(!resultPromise.isCompleted) {
       resultPromise.success(c)
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


object WorkGiverActor{
 def apply(supervisorActorRef:ActorRef[MessagesToSupervisor],
           searchID:Long,
           action:Option[SearchEnded => Unit],
           forwardResultOpt:Option[ActorRef[MessageToWorkGiver]] = None,
           verbose:Boolean = false):Behavior[MessageToWorkGiver] =
  Behaviors.setup[MessageToWorkGiver](context => new WorkGiverActor(supervisorActorRef, searchID, action, context, forwardResultOpt, verbose))
}

class WorkGiverActor(supervisor:ActorRef[MessagesToSupervisor],
                     searchID:Long,
                     action:Option[SearchEnded => Unit],
                     context:ActorContext[MessageToWorkGiver],
                     forwardResultOpt:Option[ActorRef[MessageToWorkGiver]] = None,
                     verbose:Boolean)
  extends AbstractBehavior(context:ActorContext[MessageToWorkGiver]) {

 private val resultPromise = Promise[SearchEnded]()
 private val futureForResult:Future[SearchEnded] = resultPromise.future

 action match{
  case None => ;
  case Some(a) =>
   futureForResult.onComplete({case Success(r) => a(r)})(context.executionContext)
 }

 if(verbose) context.log.info(s"created for search:$searchID")
 override def onMessage(msg: MessageToWorkGiver): Behavior[MessageToWorkGiver] = {
  msg match {
   case PromiseResult(replyTo: ActorRef[Future[SearchEnded]]) =>
    replyTo ! futureForResult
    Behaviors.same

   case c: SearchEnded =>
    require(c.searchID == searchID)
    if (c.searchID == searchID) {
     if(verbose) context.log.info(s"got result for search:$searchID : $c")
     resultPromise.success(c)
     forwardResultOpt match{
      case Some(t) => t!c
      case None => ;
     }

     c match {
      case SearchCrashed(searchID: Long, neighborhood, exception:Throwable, worker) =>
       //in this case, we avoid silent error, an report eh crash.
       if(verbose) context.log.info(s"got crash report at worker $worker for search:${c.searchID}")
      case _ => ;
     }

     Behaviors.stopped
    } else {
     //received success about another search?!
     if(verbose) context.log.error(s"got result for another search:${c.searchID}, was expecting $searchID; ignoring")
     Behaviors.same
    }

   case CancelSearch() =>
    if(verbose) context.log.info(s"received cancel search command for search:$searchID; forwarding to Supervisor")
    supervisor ! CancelSearchToSupervisor(searchID)
    Behaviors.same
  }
 }
}


abstract class MessageToTaskGiver
case class Result(result:SearchResult, taskID:Long, param:List[Int]) extends MessageToTaskGiver
case class TaskFinished(searchID:Long) extends MessageToTaskGiver
case class CancelTask() extends MessageToTaskGiver

object TaskGiverActor{
 def apply(supervisorActorRef:ActorRef[MessagesToSupervisor],
           searchID:Long,
           action:((List[Int],SearchResult)=>Unit),
           verbose:Boolean = false):Behavior[MessageToTaskGiver] =
  Behaviors.setup[MessageToTaskGiver](context => new TaskGiverActor(supervisorActorRef, searchID, action, context, verbose))
}

class TaskGiverActor(supervisor:ActorRef[MessagesToSupervisor],
                     searchID:Long,
                     action:((List[Int],SearchResult)=>Unit),
                     context:ActorContext[MessageToTaskGiver],
                     verbose:Boolean = false)
  extends AbstractBehavior(context:ActorContext[MessageToTaskGiver]) {

 if(verbose) context.log.info(s"created for search:${this.searchID}")

 override def onMessage(msg: MessageToTaskGiver): Behavior[MessageToTaskGiver] = {
  msg match {
   case Result(result:SearchResult, taskID:Long, param:List[Int]) =>
    if(taskID == this.searchID){
     action(param,result)
    } else {
     //received success about another search?!
     if(verbose) context.log.error(s"got result for another search:${taskID}, was expecting ${this.searchID}; ignoring")
    }
    Behaviors.same

   case TaskFinished(taskID) =>
    if(taskID == this.searchID){
     Behaviors.stopped
    } else {
     if(verbose) context.log.error(s"got result for another search:${taskID}, was expecting ${this.searchID}; ignoring")
     Behaviors.same
    }

   case CancelTask() =>
    if(verbose) context.log.info(s"received cancel search command for search:${this.searchID}; forwarding to Supervisor")
    supervisor ! CancelSearchToSupervisor(searchID)
    Behaviors.same
  }
 }
}

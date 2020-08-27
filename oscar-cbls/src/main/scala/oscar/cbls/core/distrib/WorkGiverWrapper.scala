package oscar.cbls.core.distrib

import akka.actor.typed.{ActorRef, ActorSystem}
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, TimeoutException}
import oscar.cbls.core.computation.Store
import oscar.cbls.core.search.SearchResult

object WorkGiverWrapper{
  def wrap(workGiverActor:ActorRef[MessageToWorkGiver],m:Store,supervisor:Supervisor):SingleWorkGiverWrapper = {
    new SingleWorkGiverWrapper(workGiverActor:ActorRef[MessageToWorkGiver], m, supervisor:Supervisor, supervisor=supervisor.system)
  }

  def andWrap(workGiverBehaviors:Array[ActorRef[MessageToWorkGiver]],m:Store,supervisor:Supervisor):AndWorkGiverWrapper =
    new AndWorkGiverWrapper(workGiverBehaviors, m, supervisor,system=supervisor.system)
}


class SingleWorkGiverWrapper(workGiverBehavior:ActorRef[MessageToWorkGiver],
                             m:Store,
                             supervisor:Supervisor,
                             implicit val system: ActorSystem[_]){
  implicit val timeout: Timeout = 30.seconds
  import akka.actor.typed.scaladsl.AskPattern._

  private val futureFuture:Future[Future[SearchEnded]] = workGiverBehavior.ask[Future[SearchEnded]](ref => PromiseResult(ref))
  private val futureResult = Await.result(futureFuture,atMost = 3.seconds)

  def getResult:SearchResult = getResultWaitIfNeeded().get

  def getResultWaitIfNeeded(timeout:Duration = Duration.Inf):Option[SearchResult] = {
    try {
      val result = Await.result[SearchEnded](futureResult,atMost=timeout)
      result match{
        case c:SearchCrashed =>
          val e = new Exception(s"Crash happened at worker:${c.worker}: \n${c.exception.getMessage}\nwhen performing neighborhood:${c.neighborhood}")
          e.setStackTrace(

            //This trims the stack trace to hide the intermediary calls to threads, futures and the like.
            (c.exception.getStackTrace.toList.reverse.dropWhile(!_.getClassName.contains("oscar.cbls")).reverse
              //c.exception.getStackTrace.toList
              ::: e.getStackTrace.toList).toArray)


          supervisor.shutdown()

          throw e
        case x:SearchCompleted => Some(x.searchResult.getLocalResult(m))
      }
    }catch{
      case _:TimeoutException => None
    }
  }

  def isResultAvailable:Boolean = futureResult.isCompleted

  def cancelComputationRequest():Unit = {
    workGiverBehavior ! CancelSearch()
  }
}

class AndWorkGiverWrapper(workGiverBehaviors:Array[ActorRef[MessageToWorkGiver]], m:Store, supervisor:Supervisor, implicit val system: ActorSystem[_]){
  implicit val timeout: Timeout = 30.seconds
  import akka.actor.typed.scaladsl.AskPattern._

  private var lastNonCompletedJob:Int = workGiverBehaviors.length-1

  private val futureFutureResults = workGiverBehaviors.map(workGiverBehavior => {
    workGiverBehavior.ask[Future[SearchEnded]](ref => PromiseResult(ref))
  })

  private val futureResults = futureFutureResults.map(futureFuture =>
    Await.result(futureFuture, Duration.Inf))

  private val results:Array[SearchResult] = Array.fill(workGiverBehaviors.length)(null)

  def getResult:SearchResult = getResultWaitIfNeeded().get

  def getResultWaitIfNeeded(timeout:Duration = Duration.Inf):Option[Array[SearchResult]] = {
    try {
      while(lastNonCompletedJob != -1) {
        Await.result[SearchEnded](futureResults(lastNonCompletedJob), atMost = timeout) match {
          case c:SearchCrashed =>
            val e = new Exception(s"Crash happened at worker:${c.worker}: \n${c.exception.getMessage}\nwhen performing neighborhood:${c.neighborhood}")
            e.setStackTrace((c.exception.getStackTrace.toList ::: e.getStackTrace.toList).toArray)

            supervisor.shutdown()
            throw e
          case r:SearchCompleted =>
            results(lastNonCompletedJob) = r.searchResult.getLocalResult(m)
            lastNonCompletedJob -= 1
        }
      }
    }catch{
      case _:TimeoutException => None
    }
    Some(results)
  }

  def isResultAvailable:Boolean = {
    getResultWaitIfNeeded(timeout = 0.seconds) match{
      case None => false
      case Some(_) => true
    }
  }

  def cancelComputationRequest():Unit = {
    for(workGiver <- workGiverBehaviors){
      workGiver ! CancelSearch()
    }
  }
}


package oscar.cbls.core.distrib

import akka.actor.typed.{ActorRef, ActorSystem}
import akka.util.Timeout
import oscar.cbls.core.computation.Store
import oscar.cbls.core.search.SearchResult

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise, TimeoutException}
import scala.util.Success

object WorkGiver {
  def wrap(workGiverActor: ActorRef[MessageToWorkGiver],
           m: Store,
           supervisor: Supervisor): WorkGiver = {
    new WorkGiver(workGiverActor: ActorRef[MessageToWorkGiver], m, supervisor: Supervisor, system = supervisor.system)
  }
}


class WorkGiver(workGiverBehavior: ActorRef[MessageToWorkGiver],
                m: Store,
                supervisor: Supervisor,
                implicit val system: ActorSystem[_]) {
  implicit val timeout: Timeout = 30.seconds

  import akka.actor.typed.scaladsl.AskPattern._

  private val futureFuture: Future[Future[SearchEnded]] =
    workGiverBehavior.ask[Future[SearchEnded]](ref => PromiseResult(ref))
  private val futureResult = Await.result(futureFuture, atMost = 3.seconds)

  def getResult: SearchResult = getResultWaitIfNeeded().get

  def getResultWaitIfNeeded(timeout: Duration = Duration.Inf): Option[SearchResult] = {
    try {
      val result = Await.result[SearchEnded](futureResult, atMost = timeout)
      result match {
        case c: SearchCrashed =>
          val e = new Exception(s"Crash happened at worker:${c.worker}: \n${c.exception.getMessage}\nwhen performing neighborhood:${c.neighborhood}")
          e.setStackTrace(

            //This trims the stack trace to hide the intermediary calls to threads, futures and the like.
            (c.exception.getStackTrace.toList.reverse.dropWhile(!_.getClassName.contains("oscar.cbls")).reverse
              //c.exception.getStackTrace.toList
              ::: e.getStackTrace.toList).toArray)


          supervisor.shutdown()

          throw e
        case x: SearchCompleted => Some(x.searchResult.getLocalResult(m))
      }
    } catch {
      case _: TimeoutException => None
    }
  }

  def isResultAvailable: Boolean = futureResult.isCompleted

  def cancelComputationRequest(): Unit = {
    workGiverBehavior ! CancelSearch()
  }

  def onResult(task: SearchEnded => Unit): Unit = {
    futureResult.onComplete({ case Success(s) => task(s) })(system.executionContext)
  }
}

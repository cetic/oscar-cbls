package oscar.cbls.core.distrib

import akka.actor.typed.{ActorRef, ActorSystem}
import akka.util.Timeout
import oscar.cbls.core.computation.Store
import oscar.cbls.core.search.SearchResult

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise, TimeoutException}
import scala.util.Success

object WorkGiver{
  def wrap(workGiverActor:ActorRef[MessageToWorkGiver],m:Store,supervisor:Supervisor):WorkGiver = {
    new WorkGiver(workGiverActor:ActorRef[MessageToWorkGiver], m, supervisor:Supervisor, system=supervisor.system)
  }

  def andWrap(workGiverBehaviors:Array[ActorRef[MessageToWorkGiver]],m:Store,supervisor:Supervisor):AndWorkGiver =
    new AndWorkGiver(workGiverBehaviors, m, supervisor,system=supervisor.system)
}


class WorkGiver(workGiverBehavior:ActorRef[MessageToWorkGiver],
                m:Store,
                supervisor:Supervisor,
                implicit val system: ActorSystem[_]){
  implicit val timeout: Timeout = 30.seconds
  import akka.actor.typed.scaladsl.AskPattern._

  private val futureFuture:Future[Future[SearchEnded]] =
    workGiverBehavior.ask[Future[SearchEnded]](ref => PromiseResult(ref))
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

  def onResult(task:SearchEnded => Unit):Unit = {
    futureResult.onComplete({case Success(s) => task(s)})(system.executionContext)
  }
}

class AndWorkGiver(workGiverBehaviors:Array[ActorRef[MessageToWorkGiver]], m:Store, supervisor:Supervisor, implicit val system: ActorSystem[_]){
  implicit val timeout: Timeout = 30.seconds
  import akka.actor.typed.scaladsl.AskPattern._

  private var lastNonCompletedJob:Int = workGiverBehaviors.length-1

  private val futureFutureResults = workGiverBehaviors.map(workGiverBehavior => {
    workGiverBehavior.ask[Future[SearchEnded]](ref => PromiseResult(ref))
  })

  private val futureResults = futureFutureResults.map(futureFuture =>
    Await.result(futureFuture, Duration.Inf))

  private val results:Array[SearchResult] = Array.fill(workGiverBehaviors.length)(null)

  def getResult:Array[SearchResult] = getResultWaitIfNeeded().get

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


class WorkStream(m:Store, supervisor:Supervisor) {

  private val resultPromise = Promise[Option[SearchCrashed]]()
  private val futureForResult: Future[Option[SearchCrashed]] = resultPromise.future

  //we start with one more unit than the expected one.
  @volatile
  private final var remainingToDos = 1

  def addWork(search:SearchRequest,task:SearchResult => Unit): Unit ={
    this.synchronized {
      remainingToDos += 1
    }
    supervisor.delegateWithAction(search,execute(task,_))
  }

  def waitAllComplete() {
    this.synchronized {
      remainingToDos -= 1
      if (remainingToDos == 0
        && !resultPromise.isCompleted) {
        resultPromise.complete(Success(None))
      }
    }

    Await.result(futureForResult, Duration.Inf) match {
      case Some(c: SearchCrashed) =>
        val e = new Exception(s"Crash happened at worker:${c.worker}: \n${c.exception.getMessage}\nwhen performing neighborhood:${c.neighborhood}")
        e.setStackTrace(

          //This trims the stack trace to hide the intermediary calls to threads, futures and the like.
          (c.exception.getStackTrace.toList.reverse.dropWhile(!_.getClassName.contains("oscar.cbls")).reverse
            //c.exception.getStackTrace.toList
            ::: e.getStackTrace.toList).toArray)

        supervisor.shutdown()

        throw e
      case None => ;
    }
  }

  private def execute(task: SearchResult => Unit, arg: SearchEnded): Unit = {
    this.synchronized {
      remainingToDos -= 1

      arg match {
        case SearchCompleted(_, searchResult) =>
          task(searchResult.getLocalResult(m))
          if (remainingToDos == 0 && !resultPromise.isCompleted) {
            resultPromise.complete(Success(None))
          }
        case _: SearchAborted =>
          if (remainingToDos == 0 && !resultPromise.isCompleted) {
            resultPromise.complete(Success(None))
          }
        case crashed: SearchCrashed =>
          if (!resultPromise.isCompleted) {
            resultPromise.complete(Success(Some(crashed)))
          }
      }
    }
  }
}


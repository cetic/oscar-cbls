package oscar.cbls.core.distrib

import akka.actor.typed._
import akka.util.Timeout
import org.slf4j.{Logger, LoggerFactory}
import oscar.cbls.core.computation.{Solution, Store}
import oscar.cbls.core.objective.{FunctionObjective, IndependentObjective, Objective}
import oscar.cbls.core.search.{DoNothingMove, Move}

import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.util.Random
/*
object Test extends App {

  val startLogger: Logger = LoggerFactory.getLogger("SupervisorObject")
  startLogger.info("starting actor system")

  val supervisorActor: ActorSystem[MessagesToSupervisor] = Supervisor.internalStartSupervisorAndActorSystem(verbose = false, tic = 1.seconds)
  implicit val ec: ExecutionContext = supervisorActor.executionContext
  implicit val timeout: Timeout = Timeout(3.seconds)
  val supervisor = Supervisor.wrapSupervisor(supervisorActor, new Store(), false)(supervisorActor)
  val requests2: Array[SearchRequest] = Array.tabulate(10) {
    i =>
      SearchRequest(
        RemoteNeighborhoodIdentification(i % 2, parameters = List(i), s"or-searching ${i % 2} param:$i"),
        _ > _,
        new IndependentObjective {
          override def toString: String = "objective"

          override def convertToObjective(m: Store): Objective = new FunctionObjective(() => 6)
        },
        startSolution = IndependentSolution(Solution(List.empty, null)))
  }
  val w2 = supervisor.delegateSearchesStopAtFirst(requests2)
  val requests3: Array[SearchRequest] = Array.tabulate(10) {
    i =>
      SearchRequest(
        RemoteNeighborhoodIdentification(i % 2, parameters = List(i), s"or-searching ${i % 2} param:$i"),
        _ > _,
        new IndependentObjective {
          override def toString: String = "objective"

          override def convertToObjective(m: Store): Objective = new FunctionObjective(() => 7)
        },
        startSolution = IndependentSolution(Solution(List.empty, null)))
  }
  val w3 = supervisor.delegateSearchesStopAtFirst(requests3)

  createWorker("grincheux")
  createWorker("timide")
  createWorker("dormeur")
  createWorker("joyeux")
  createWorker("prof")
  createWorker("atchoum")

  /*
    for(i <- 0 until Worker.nbCores){
      createWorker("worker_"+i,supervisorActor)
    }*/

  for (neighborhoodID <- 0 until 5) {
    supervisor.delegateSearch(SearchRequest(
      RemoteNeighborhoodIdentification(neighborhoodID % 2, parameters = List(neighborhoodID), s"searching ${neighborhoodID % 2} param:$neighborhoodID"),
      _ > _,
      new IndependentObjective {
        override def toString: String = "objective"

        override def convertToObjective(m: Store): Objective = new FunctionObjective(() => 5)
      },
      startSolution = IndependentSolution(Solution(List.empty, null))))
  }

  def createWorker(actorName: String): Unit = {
    val neighborhoods: SortedMap[Int, RemoteNeighborhood] = createNeighborhoods(actorName)
    supervisor.createLocalWorker(Store(), neighborhoods)
  }

  def createNeighborhoods(actorName: String): SortedMap[Int, RemoteNeighborhood] = {
    SortedMap(
      (0, createNeighborhood(actorName, 0)),
      (1, createNeighborhood(actorName, 1)),
      (2, createNeighborhood(actorName, 2))
    )
  }

  def createNeighborhood(actorName: String, neighborhoodID: Int): RemoteNeighborhood = {
    new RemoteNeighborhood(neighborhoodID, null) {
      //this is for test purpose
      override def explore(parameters: List[Long],
                           obj: Objective,
                           acc: (Long, Long) => Boolean,
                           shouldAbort: () => Boolean): IndependentSearchResult = {

        val taskDuration = 500 * (1 + parameters.head) //ms
        val startTime = java.lang.System.currentTimeMillis()
        val endTime = startTime + taskDuration
        var i: Long = 0
        var continue = true
        var aborted = false
        while (continue) {
          if (java.lang.System.currentTimeMillis() >= endTime) {
            continue = false
          }
          if (shouldAbort()) {
            continue = false
            aborted = true
          }
          i = i + 1
        }
        if (aborted)
          IndependentNoMoveFound()
        else if (Random.nextBoolean())
          IndependentMoveFound(PseudoMove(s"neighborhoodID:${neighborhoodID} params:${parameters} i:$i").getIndependentMove(null))
        else {
          // throw new Exception("bug")
          IndependentNoMoveFound()
        }
      }
    }
  }

  case class PseudoMove(test: String) extends Move(neighborhoodName = "PseudoNeighborhood") {
    override def commit(): Unit = {
    }

    override def getIndependentMove(m: Store): IndependentMove = new IndependentMove() {

      override def makeLocal(m: Store): Move = DoNothingMove(0)

      override def objAfter: Long = Long.MaxValue

      override def neighborhoodName: String = s"IndependentMove($test)"

      override def toString: String = neighborhoodName
    }
  }

  println("got result2:" + w2.getResultWaitIfNeeded())

  createWorker("simplet")

  //starting more workers, to check if it works
  println("got result3:" + w3.getResultWaitIfNeeded())


  supervisor.shutdown()

  println("exit")

}
*/


package oscar.cbls.core.distrib

import akka.actor.typed._
import akka.util.Timeout
import org.slf4j.{Logger, LoggerFactory}
import oscar.cbls.core.computation.{Solution, Store}
import oscar.cbls.core.objective.{FunctionObjective, Objective}
import oscar.cbls.core.search.{DoNothingMove, Move, MoveFound, NoMoveFound, SearchResult}

import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.util.Random

object Test extends App{

  val  startLogger:Logger = LoggerFactory.getLogger("SupervisorObject");
  startLogger.info("starting actor system")

  val supervisorActor:ActorSystem[MessagesToSupervisor] = Supervisor.internalStartSupervisorAndActorSystem(verbose = false, tic = 1.seconds)
  implicit val ec: ExecutionContext = supervisorActor.executionContext
  implicit val timeout: Timeout = Timeout(3.seconds)
  val supervisor = Supervisor.wrapSupervisor(supervisorActor, new Store(), false)(supervisorActor)

  case class PseudoMove(test:String) extends Move(neighborhoodName="PseudoNeighborhood"){
    override def commit(): Unit = {
    }

    override def getIndependentMove(m: Store): IndependentMove = new IndependentMove(){

      override def makeLocal(m: Store): Move = DoNothingMove(0)

      override def objAfter: Long = Long.MaxValue

      override def neighborhoodName: String = s"IndependentMove($test)"

      override def toString: String = neighborhoodName
    }
  }

  def createNeighborhood(actorName:String,neighborhoodID:Int):RemoteNeighborhood = {
    new RemoteNeighborhood(neighborhoodID, null) {
      //this is for test purpose
      override def explore(parameters: List[Long],
                           obj: Objective,
                           acc: (Long,Long) => Boolean,
                           shouldAbort: () => Boolean): SearchResult = {

        val taskDuration = 500*(1+parameters.head) //ms
        val startTime = java.lang.System.currentTimeMillis()
        val endTime = startTime + taskDuration
        var i:Long = 0
        var continue = true
        var aborted = false
        while(continue){
          if(java.lang.System.currentTimeMillis() >= endTime){
            continue = false
          }
          if(shouldAbort()) {
            continue = false
            aborted = true
          }
          i = i + 1
        }
        if(aborted)
          NoMoveFound
        else if(Random.nextBoolean())
          MoveFound(PseudoMove(s"neighborhoodID:${neighborhoodID} params:${parameters} i:$i"))
        else {
          // throw new Exception("bug")
          NoMoveFound
        }
      }
    }
  }

  def createNeighborhoods(actorName:String):SortedMap[Int,RemoteNeighborhood] = {
    SortedMap(
      (0,createNeighborhood(actorName,0)),
      (1,createNeighborhood(actorName,1)),
      (2,createNeighborhood(actorName,2))
    )
  }

  def createWorker(actorName:String): Unit ={
    val neighborhoods:SortedMap[Int,RemoteNeighborhood] = createNeighborhoods(actorName)
    supervisor.createLocalWorker(new Store(),neighborhoods)
  }

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

  for(neighborhoodID <- 0 until 5){
    supervisor.delegateSearch(SearchRequest(
      RemoteNeighborhoodIdentification(neighborhoodID % 2,parameters = List(neighborhoodID),s"searching ${neighborhoodID%2} param:$neighborhoodID"),
      _ > _,
      new IndependentOBj {
        override def toString: String = "objective"
        override def convertToOBj(m: Store): Objective = new FunctionObjective(() => 5)
      },
      startSolution = IndependentSolution(Solution(List.empty,null))))
  }

  println("started first round")


  val requests2:Array[SearchRequest] = Array.tabulate(10){
    i => SearchRequest(
      RemoteNeighborhoodIdentification(i % 2,parameters = List(i),s"or-searching ${i%2} param:$i"),
      _ > _,
      new IndependentOBj {
        override def toString: String = "objective"
        override def convertToOBj(m: Store): Objective = new FunctionObjective(() => 6)
      },
      startSolution = IndependentSolution(Solution(List.empty,null)))
  }


  val w2 = supervisor.delegateSearchesStopAtFirst(requests2)

  val requests3:Array[SearchRequest] = Array.tabulate(10){
    i => SearchRequest(
      RemoteNeighborhoodIdentification(i % 2,parameters = List(i),s"or-searching ${i%2} param:$i"),
      _ > _,
      new IndependentOBj {
        override def toString: String = "objective"
        override def convertToOBj(m: Store): Objective = new FunctionObjective(() => 7)
      },
      startSolution = IndependentSolution(Solution(List.empty,null)))
  }

  val w3 = supervisor.delegateSearchesStopAtFirst(requests3)

  val workGivers4 = Array.tabulate(10){
    i => SearchRequest(
      RemoteNeighborhoodIdentification(i % 2,parameters = List(i),s"and-searching ${i%2} param:$i"),
      _ > _,
      new IndependentOBj {
        override def toString: String = "objective"
        override def convertToOBj(m: Store): Objective = new FunctionObjective(() => 4)
      },
      startSolution = IndependentSolution(Solution(List.empty,null)))
  }

  val w4 = supervisor.delegateSearches(workGivers4)

  println("got result2:" +   w2.getResultWaitIfNeeded())

  createWorker("simplet")

  //starting more workers, to check if it works
  println("got result3:" +   w3.getResultWaitIfNeeded())

  println("got result4:" +   w4.getResultWaitIfNeeded().map("\n\t" + _.mkString("\n\t")))

  supervisor.shutdown()

  println("exit")

}



package oscar.cbls.core.distrib

import akka.actor.typed._
import akka.util.Timeout
import org.slf4j.{Logger, LoggerFactory}
import oscar.cbls.core.computation.{Solution, Store}
import oscar.cbls.core.objective.{FunctionObjective, Objective}
import oscar.cbls.core.search.{Move, MoveFound, NoMoveFound, SearchResult}

import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.util.Random

object Test extends App{

  val  startLogger:Logger = LoggerFactory.getLogger("SupervisorObject");
  startLogger.info("starting actor system")

  val supervisorActor:ActorSystem[MessagesToSupervisor] = Supervisor.startSupervisorAndActorSystem(verbose = false, tic = 1.seconds)
  implicit val ec: ExecutionContext = supervisorActor.executionContext
  implicit val timeout: Timeout = Timeout(3.seconds)
  val supervisor = Supervisor.wrapSupervisor(supervisorActor, false)(supervisorActor)

  case class PseudoMove(test:String) extends Move(){
    override def commit(): Unit = {
    }

    override def getIndependentMove(m: Store): IndependentMove = new IndependentMove(){
      override def commit(m: Store): Unit = {}

      override def objAfter: Long = Long.MaxValue

      override def neighborhoodName: String = s"IndependentMove($test)"
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
    supervisor.createLocalWorker(neighborhoods,new Store())
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
      startSolution = new Solution(List.empty,null)))
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
      startSolution = new Solution(List.empty,null))
  }


  val w2 = WorkGiverWrapper.wrap(supervisor.delegateORSearches(requests2),null,supervisor)(supervisorActor)

  val requests3:Array[SearchRequest] = Array.tabulate(10){
    i => SearchRequest(
      RemoteNeighborhoodIdentification(i % 2,parameters = List(i),s"or-searching ${i%2} param:$i"),
      _ > _,
      new IndependentOBj {
        override def toString: String = "objective"
        override def convertToOBj(m: Store): Objective = new FunctionObjective(() => 7)
      },
      startSolution = new Solution(List.empty,null))
  }

  val w3 = WorkGiverWrapper.wrap(supervisor.delegateORSearches(requests3),null,supervisor)(supervisorActor)

  val workGivers4 = Array.tabulate(10){
    i => supervisor.delegateSearch(SearchRequest(
      RemoteNeighborhoodIdentification(i % 2,parameters = List(i),s"and-searching ${i%2} param:$i"),
      _ > _,
      new IndependentOBj {
        override def toString: String = "objective"
        override def convertToOBj(m: Store): Objective = new FunctionObjective(() => 4)
      },
      startSolution = new Solution(List.empty,null)))
  }

  val w4 = WorkGiverWrapper.andWrap(workGivers4,null,supervisor)

  println("got result2:" +   w2.getResultWaitIfNeeded())

  createWorker("simplet")

  //starting more workers, to check if it works
  println("got result3:" +   w3.getResultWaitIfNeeded())

  println("got result4:" +   w4.getResultWaitIfNeeded().map("\n\t" + _.mkString("\n\t")))

  supervisor.shutdown()

  println("exit")

}



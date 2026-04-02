package oscar.cbls.test.core.distributed

import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, ActorSystem, Behavior, SupervisorStrategy}
import org.apache.pekko.util.Timeout
import org.slf4j.{Logger, LoggerFactory}
import oscar.cbls.core.computation.Store
import oscar.cbls.core.distributed.actors.{Supervisor, Worker}
import oscar.cbls.core.distributed.computation.{Aborted, ActualResult, SearchConnector, Task, TaskParameters}
import oscar.cbls.core.distributed.protocol._
import oscar.cbls.core.distributed.search.RemotelyCallableTask

import scala.concurrent.duration.DurationInt
import scala.util.Random

object WorkerSupervisorTest extends App {

  val startLogger: Logger = LoggerFactory.getLogger("RootDemoActor")
  ActorSystem(
    Behaviors.setup[MessageToTest](context => startTest(context, startLogger)),
    "RootDemoActor"
  )

  val startTime = System.currentTimeMillis()
  val endTime   = startTime + 10000
  def createDumbComputationSupport(screwChecksum: Boolean, nbTasks: Int): SearchConnector = {
    val store = new Store()
    store.close()
    val storeAdapter = new SearchConnector(store = store, List.empty)

    for (i <- 0 until nbTasks) {
      // this bypasses the searchConnector to declare a dumbTask
      storeAdapter.declareRemotelyCallableTask(createDumbRemoteTask(i))
    }

    storeAdapter
  }

  case class DumbActualResult(aValue: Int) extends ActualResult {}

  case class DumbTaskDescription(taskClass: Int, param: Int) extends TaskParameters
  def createDumbRemoteTask(taskClass: Integer): RemotelyCallableTask = new RemotelyCallableTask {

    override def performTask(
      description: TaskParameters,
      shouldStop: () => Boolean,
      computationSupport: SearchConnector,
      log: Logger,
      verbosityLevel: Int,
      taskId: Long,
      taskClass: Int
    ): ActualResult = {
      val param = description.asInstanceOf[DumbTaskDescription]
      require(param.taskClass == taskClass)
      for (i <- 0 to param.param) {
        Thread.sleep(20)
        if (shouldStop()) return Aborted
      }
      DumbActualResult(Random.nextInt(100))
    }
  }

  def createDumbWorker(
    supervisor: ActorRef[MessageToSupervisor],
    workerId: Int,
    screwStore: Boolean,
    nbTasks: Int
  ): Behavior[MessageToWorker] = {
    Worker.apply(
      supervisor,
      "Worker" + workerId,
      createDumbComputationSupport(screwStore, nbTasks),
      verbosityLevel = 1,
      testBehaviorOpt = None
    )
  }
  abstract class MessageToTest
  object SearchFinished extends MessageToTest
  object DoSomething    extends MessageToTest

  def startTest(context: ActorContext[MessageToTest], start: Logger): Behavior[MessageToTest] = {

    val nbTasks = 10

    val supervisor =
      context.spawn(
        Behaviors
          .supervise(Supervisor.apply(createDumbComputationSupport(screwChecksum = false, nbTasks), 0))
          .onFailure(SupervisorStrategy.stop),
        "supervisor"
      )

    val workers: Array[Option[ActorRef[MessageToWorker]]] = Array.tabulate(10)(workerId => {
      if (Random.nextBoolean())
        Some(
          context.spawn(
            createDumbWorker(supervisor, workerId, screwStore = false, nbTasks = nbTasks),
            "Worker" + workerId
          )
        )
      else None
    })

    val log = context.log
    context.scheduleOnce(3.seconds, context.self, DoSomething)
    runTest(supervisor, workers, searchActor = false, nbTasks = nbTasks, log)
  }

  def runTest(
    supervisor: ActorRef[MessageToSupervisor],
    workers: Array[Option[ActorRef[MessageToWorker]]],
    searchActor: Boolean,
    nbTasks: Int,
    log: Logger
  ): Behavior[MessageToTest] = {

    Behaviors.receive { (testContext: ActorContext[MessageToTest], command) =>
      try {
        if (System.currentTimeMillis() >= endTime) {
          System.out.println("exit test after 10 seconds")
          System.exit(0)
        }
        command match {

          case DoSomething =>
            log.info("DoSomething")
            testContext.scheduleOnce(3.seconds, testContext.self, DoSomething)
            Random.nextInt(2) match {
              case 0 => // start a search if none
                if (!searchActor) {
                  // start a new search
                  log.info("starting a new search, randomly")

                  createDumbSearch(supervisor, test = testContext.self, nbTasks)
                  log.info("dumb search created")
                  runTest(supervisor, workers, searchActor = true, nbTasks, log)

                } else {
                  Behaviors.same
                }
              case 1 => // spawn new worker
                log.info("creating new worker, randomly")
                val freeWorker = workers.indexWhere(_.isEmpty, 0)
                if (freeWorker != -1) {
                  // spawn worker and continue; ok it is mutable and dirty; so what?

                  workers(freeWorker) = Some(
                    testContext.spawn(
                      createDumbWorker(
                        supervisor: ActorRef[MessageToSupervisor],
                        freeWorker,
                        screwStore = false,
                        nbTasks
                      ),
                      "worker" + freeWorker
                    )
                  )
                }
                Behaviors.same
              case _ =>
                Behaviors.same
            }

          case SearchFinished =>
            runTest(supervisor, workers, searchActor = false, nbTasks, log)
        }
      } catch {
        case r: Throwable =>
          log.info(r.toString + r.getStackTrace.mkString("\n"))
          Behaviors.stopped
      }
    }
  }

  def createDumbSearch(
    supervisor: ActorRef[MessageToSupervisor],
    test: ActorRef[MessageToTest],
    nbTasks: Int
  ): Unit = {
    implicit val askTimeout: Timeout = org.apache.pekko.util.Timeout(5.seconds)

    supervisor ! SpawnSearchActorAndStatusRequest(Behaviors.setup[MessageToSearch](context => {
      val dumbSearchLogger: Logger = LoggerFactory.getLogger("dumbSearch")
      supervisor ! GetNewUniqueTaskIds(context.self, 10, context.self)
      runDumbSearch(context, supervisor, nbTasks, dumbSearchLogger, List.empty, test)
    }))
  }

  def runDumbSearch(
    context: ActorContext[MessageToSearch],
    supervisor: ActorRef[MessageToSupervisor],
    nbTasks: Int,
    logger: Logger,
    tasks: List[(Int, Option[Int])],
    test: ActorRef[MessageToTest]
  ): Behavior[MessageToSearch] = {

    // run some random searches
    Behaviors.receive { (context: ActorContext[MessageToSearch], command: MessageToSearch) =>
      logger.info("dumb search received " + command)

      command match {
        case NewUniqueTaskIds(firstTaskId, lastTaskId) =>
          if (firstTaskId > 1000) {
            // finish the test
            logger.info("Finished DumbSearch")
            supervisor ! CancelAllMyRemainingTasks(context.self, searchFinished = true)
            test ! SearchFinished
            Behaviors.stopped
          } else if (firstTaskId != lastTaskId) {
            logger.info("got multiple NewUniqueTaskId")
            supervisor ! CreateTasks(
              context.self,
              (firstTaskId to lastTaskId).toList.map((taskId: Long) =>
                (
                  Task(
                    taskId,
                    (taskId % 10).toInt,
                    DumbTaskDescription((taskId % 10).toInt, (taskId % 5).toInt)
                  ),
                  context.self
                )
              )
            )
            logger.info("dumb search being dumb again")
            Behaviors.same
          } else {
            logger.info("got multiple NewUniqueTaskId")
            val taskId = firstTaskId
            supervisor ! CreateTask(
              context.self,
              Task(
                taskId,
                (taskId % 10).toInt,
                DumbTaskDescription((taskId % 10).toInt, (taskId % 5).toInt)
              ),
              context.self
            )
            logger.info("dumb search being dumb again")
            Behaviors.same
          }

        case StatusReport(nbWorkers, nbBusy, nbIdle) =>
          logger.info("got status")
          logger.info("dumb search being dumb again")
          Behaviors.same
        case ResultObtained(result) =>
          logger.info("got ResultObtained")
          val dumbResult = result.result.asInstanceOf[DumbActualResult]
          if (dumbResult.aValue > 50) {
            supervisor ! CancelAllMyRemainingTasks(context.self, searchFinished = false)
            supervisor ! GetNewUniqueTaskIds(context.self, 10, context.self)
          }

          logger.info("dumb search being dumb again")
          Behaviors.same
        case Crash(error) =>
          logger.error(error.toString)
          return Behaviors.stopped
        case _ =>
          logger.info("dumb search being dumb again")
          Behaviors.same
      }
    }
  }
}

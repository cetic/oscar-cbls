package oscar.cbls.core.distributed.actors

import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.slf4j.{Logger, LoggerFactory}
import oscar.cbls.core.distributed.computation._
import oscar.cbls.core.distributed.protocol._
import oscar.cbls.core.distributed.search.TestBehavior

import java.util.concurrent.{LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** The main class of the Worker
  */
object Worker {

  /** Instantiate a Worker behavior.
    *
    * The behavior of a worker is best described by the messages it accepts, which are subclasses of
    * [[oscar.cbls.core.distributed.protocol.MessageToWorker]].
    *
    * @param supervisor
    *   the supervisor actor that the worker must contact to have tasks
    * @param workerName
    *   the name of the worker, used for log, so if it is not unique it does not really matter
    * @param computationSupport
    *   the ComputationSupport containing all computation-related structures that the worker will
    *   use to perform its tasks
    * @param verbosityLevel
    *   the verbosity level of the worker, will be passed to the tasks together with a logger
    * @param testBehaviorOpt
    *   optional test behavior for simulating crashes/disconnections
    * @param workerNode
    *   optional reference to the WorkerNode that spawned this worker (None for local workers)
    * @return
    *   the behavior of a worker, to spawn in a context
    */
  def apply(
    supervisor: ActorRef[MessageToSupervisor],
    workerName: String,
    computationSupport: SearchConnector,
    verbosityLevel: Int,
    testBehaviorOpt: Option[TestBehavior],
    workerNode: Option[ActorRef[MessageToWorkerNode]] = None
  ): Behavior[MessageToWorker] = {

    val log: Logger = LoggerFactory.getLogger(workerName)

    if (testBehaviorOpt.isDefined)
      log.info(s"Starting worker $workerName with testBehavior:${testBehaviorOpt.get}")
    else if (verbosityLevel >= 1) log.info(s"Starting worker $workerName")

    Behaviors.setup[MessageToWorker](context => {

      testBehaviorOpt match {
        case Some(t) if t.delayStartupByMs >= 0 =>
          log.info(s"test behavior; delaying worker join by ${t.delayStartupByMs}ms")
          context.system.scheduler.scheduleOnce(
            FiniteDuration(t.delayStartupByMs, MILLISECONDS),
            () => {
              log.info("test behavior; delayed worker join")
              supervisor !
                WorkerRegister(
                  fromWorker = context.self,
                  modelChecksum = computationSupport.checkSumOnStoreAndSearch,
                  workerNode = workerNode
                )
            }
          )

        case _ =>
          supervisor ! WorkerRegister(
            fromWorker = context.self,
            modelChecksum = computationSupport.checkSumOnStoreAndSearch
          )
      }
      new Worker(
        context,
        supervisor,
        computationSupport,
        verbosityLevel = verbosityLevel,
        log = log,
        name = workerName,
        testBehaviorOpt = testBehaviorOpt
      ).doIt(List.empty)

    })
  }
}

case class InternalTaskCompleted(task: Task, result: Try[ActualResult])
    extends InternalMessageToWorker

class Worker(
  context: ActorContext[MessageToWorker],
  supervisor: ActorRef[MessageToSupervisor],
  computationSupport: SearchConnector,
  verbosityLevel: Int,
  log: Logger,
  name: String,
  testBehaviorOpt: Option[TestBehavior]
) {

  /** Internal message that tne worker sends to itself to notify that a task has been completed.
    * @param task
    *   the [[Task]] object
    * @param result
    *   the result of the task
    */
  case class InternalTaskCompleted(task: Task, result: Try[ActualResult])
      extends InternalMessageToWorker

  private val startupTime: Long = System.currentTimeMillis() + (testBehaviorOpt match {
    case Some(t) => t.delayStartupByMs; case None => 0
  })

  private class ShouldStop(@volatile var shouldStop: Boolean = false)

  /** The possible status of a task, as written by the computation thread.
    */
  abstract sealed class TaskStatus

  private case object Waiting extends TaskStatus

  private case object Cancelled extends TaskStatus

  private case class Started(startTime: Long) extends TaskStatus

  private case class Finished(startTime: Long, endTime: Long) extends TaskStatus

  private var nbStartedTasks: Int  = 0
  private var nbFinishedTasks: Int = 0

  private val myService = new ThreadPoolExecutor(
    1,
    1,
    0L,
    TimeUnit.MILLISECONDS,
    new LinkedBlockingQueue[Runnable],
    new ThreadPoolExecutor.DiscardPolicy() // We use this policy because the Future create an error message when performing a coordinated shutdown.
  )

  private val myContext = ExecutionContext.fromExecutor(myService)

  /** a tash handle that allows interacting with a task once it is queued to the system thread
    * @param shouldStop
    *   a method that the thread can query to potentially stop tje computation
    * @param status
    *   a status that the thread updates with the status of the computation.
    */
  private case class TaskHandle(shouldStop: ShouldStop, @volatile var status: TaskStatus)

  /** Enqueues the new task into the system thread and returns a handle so that it can be stopped
    * and its status can be monitored through the shared status variable
    * @param startedTask
    *   the task that should be started
    * @return
    *   a task handle to potentially cancel the task and have a status of the task
    */
  private def enqueueTaskToThread(startedTask: Task): TaskHandle = {
    nbStartedTasks += 1
    val algo          = computationSupport.remotelyCallableTasks(startedTask.taskClass)
    val theShouldStop = new ShouldStop()
    val handle        = TaskHandle(theShouldStop, status = Waiting)
    Future {
      val startTime = System.currentTimeMillis()

      val toReturn = if (!handle.shouldStop.shouldStop) {
        handle.status = Started(startTime)
        algo.performTask(
          startedTask.taskDescription,
          () => theShouldStop.shouldStop,
          computationSupport,
          log,
          verbosityLevel,
          taskId = startedTask.taskId,
          taskClass = startedTask.taskClass
        )
      } else {
        handle.status = Cancelled
        Aborted
      }
      val endTime = System.currentTimeMillis()
      handle.status = Finished(startTime, endTime)
      if (handle.shouldStop.shouldStop) {
        handle.status = Cancelled
        Aborted
      } else {
        toReturn
      }
    }(myContext).onComplete { result =>
      // we split into two steps in order to catch errors on the first step;
      // assuming the line here below will not crash...
      context.self ! InternalTaskCompleted(startedTask, result)
    }(myContext)
    handle
  }

  /** Sends a status report to the supervisor.
    * @param todoList
    *   the tasks that are in the queue of the thread and their handle
    */
  private def sendStatusReport(todoList: List[(Task, TaskHandle)]): Unit = {

    val ongoingTasks = todoList.flatMap(t =>
      t._2.status match {
        case s: Started => Some((t._1, s))
        case _          => None
      }
    )

    val waitingTasks = todoList.flatMap(t =>
      t._2.status match {
        case Waiting if !t._2.shouldStop.shouldStop => Some(t._1)
        case _                                      => None
      }
    )

    if (ongoingTasks.isEmpty) {
      supervisor ! WorkerStatusReport(
        fromWorker = context.self,
        ongoingTaskAndTimeSpent = None,
        toDoList = waitingTasks.map(_.taskId),
        nbStartedTasks = nbStartedTasks,
        nbFinishedTasks = nbFinishedTasks
      )
    } else {
      val startTime = ongoingTasks.head._2.startTime
      val taskId    = ongoingTasks.head._1.taskId

      supervisor ! WorkerStatusReport(
        fromWorker = context.self,
        ongoingTaskAndTimeSpent = Some((taskId, System.currentTimeMillis() - startTime)),
        toDoList = waitingTasks.map(_.taskId),
        nbStartedTasks = nbStartedTasks,
        nbFinishedTasks = nbFinishedTasks
      )
    }
  }

  /** Executes the test behavior in parameter of this class; check [[TestBehavior]]
    * @return
    *   false if the actor should be stopped through Behaviors.stopped
    */
  private def testBehaviorFalseIfStopped(): Boolean = {
    testBehaviorOpt match {
      case Some(t: TestBehavior) =>
        val currentTime = System.currentTimeMillis()
        if (t.crashAfterMs >= 0 && startupTime + t.crashAfterMs < currentTime) {
          log.info("test behavior; simulating crash")
          throw new Error("""This is a test error thrown by the worker;
                            |since it reflects a bug in the algo it should trigger a global crash
                            |and this message should be reported in the stack trace""".stripMargin)
        } else if (t.disconnectAfterMs >= 0 && startupTime + t.disconnectAfterMs < currentTime) {
          log.info("test behavior; simulating disconnection")
          supervisor ! WorkersDisconnected(List(context.self))
          true
        } else {
          false
        }
      case None => false
    }
  }

  /** The single task defining the worker. The main principle is that each task is enqueued into the
    * system thread.
    * @param toDoList
    *   the tasks tht have been received. This list serves as a communication between the system
    *   thread and the actor.
    * @return
    *   the next behavior of the worker
    */
  private def doIt(toDoList: List[(Task, TaskHandle)]): Behavior[MessageToWorker] = {
    Behaviors.receive { (_, command) =>
      try {
        if (testBehaviorFalseIfStopped()) {
          Behaviors.stopped
        } else {
          if (verbosityLevel >= 2) log.info("received " + command)
          command match {
            case EnqueueTask(task) =>
              require(
                !toDoList.exists(_._1.taskId == task.taskId),
                "enqueuing duplicate tasks: " + task.taskId + " currently in queue: " +
                  toDoList.map(_._1.taskId).mkString(",")
              )
              val newTodo1 = toDoList ::: List((task, enqueueTaskToThread(task)))

              sendStatusReport(newTodo1)

              doIt(newTodo1)
            case KillTasks(taskIDs) =>
              val taskSet = taskIDs.toSet
              for (task <- toDoList if taskSet contains task._1.taskId) {
                task._2.shouldStop.shouldStop = true
              }

              doIt(toDoList)

            case WorkerStatusRequest() =>
              sendStatusReport(toDoList)
              Behaviors.same

            case WorkerShutdown(msgOpt) =>
              if (verbosityLevel >= 1) {
                log.info("WorkerShutdown: " + msgOpt.getOrElse(""))
              }

              myService.shutdownNow()
              Behaviors.stopped

            case InternalTaskCompleted(task, result) =>
              nbFinishedTasks += 1

              result match {
                case Failure(exception) =>
                  supervisor ! WorkerCrash(
                    fromWorker = context.self,
                    ongoingTaskAndType = Some((task.taskId, -1)),
                    exception
                  )
                  myService.shutdownNow()
                  Behaviors.stopped

                case Success(Aborted) =>
                  supervisor ! WorkerTaskCancelled(
                    fromWorker = context.self,
                    cancelledTaskId = task.taskId
                  )

                  doIt(toDoList.filter(_._1.taskId != task.taskId))

                case Success(actualResult) =>
                  toDoList.find(_._1.taskId == task.taskId) match {
                    case None =>
                      log.error(
                        "task completed when not running; was probably cancelled; ignored taskId:" + task.taskId
                      )
                      Behaviors.same

                    case Some((task, taskHandle)) =>
                      if (taskHandle.shouldStop.shouldStop) {
                        // was cancelled
                        supervisor ! WorkerTaskCancelled(
                          fromWorker = context.self,
                          cancelledTaskId = task.taskId
                        )

                        doIt(toDoList.filter(_._1.taskId != task.taskId))
                      } else {
                        if (verbosityLevel == 2) log.info("completed task " + task.taskId)
                        taskHandle.status match {
                          case Finished(startTime, endTime) =>
                            val timeSpendMs = endTime - startTime
                            supervisor ! WorkerTaskFinished(
                              fromWorker = context.self,
                              result = TaskResult(
                                taskId = task.taskId,
                                taskClass = task.taskClass,
                                timeSpentMs = timeSpendMs,
                                result = actualResult
                              )
                            )

                            doIt(toDoList.filter(_._1.taskId != task.taskId))

                          case x =>
                            throw new Error("unexpected message " + x)
                        }
                      }
                  }
              }
            case _: InternalMessageToWorker =>
              // this is an abstract class, and it should not be received as is,
              // but scala compiler complains if it is not here
              Behaviors.same
          }
        }
      } catch {
        case r: Throwable =>
          log.error(r.toString + r.getStackTrace.mkString("\n"))
          supervisor ! WorkerCrash(fromWorker = context.self, ongoingTaskAndType = None, r)
          myService.shutdownNow()
          Behaviors.stopped
      }
    }
  }
}

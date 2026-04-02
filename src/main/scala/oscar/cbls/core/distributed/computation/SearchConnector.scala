package oscar.cbls.core.distributed.computation

import org.apache.pekko.actor.typed.ActorRef
import oscar.cbls.Neighborhood
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.integer.{IntSavedValue, IntVariable}
import oscar.cbls.core.computation.objective.Objective
import oscar.cbls.core.computation.seq.{SeqSavedValue, SeqVariable}
import oscar.cbls.core.computation.set.{SetSavedValue, SetVariable}
import oscar.cbls.core.computation.{Solution, Store, Variable}
import oscar.cbls.core.distributed.protocol.MessageToSupervisor
import oscar.cbls.core.distributed.search.RemotelyCallableTask
import oscar.cbls.core.search.Move

import scala.collection.immutable.HashSet

/** This class contains everything a Combinator, a Worker or a Supervisor need in order to execute
  * the search procedure at hand. It basically has a set of 'detach' (res. 'attach') methods to
  * detach (resp. attach) a variable, objective or solution from (res. to) a store
  * @param store
  *   the store, expected to be different instances representing exactly the same model
  * @param searches
  *   the search procedures that will be executed during the search. Only the root searches should
  *   be mentioned here.
  */
class SearchConnector(val store: Store, searches: List[Neighborhood]) {
  var supervisor: ActorRef[MessageToSupervisor] = _

  /** This method is to compare two stores in order to know if hey represent the same optimization
    * problem or not, for use in distributed optimization
    * @return
    *   a checksum on the store structure
    */
  val checkSumOnStoreAndSearch: Long = {
    require(store.isClosed, "Model must be closed before calling checkSumOnStoreStructure")

    val storeHash: Long = store.variables.values.toList
      .map(v =>
        if (v.isADecisionVariable) {
          0L + (v.name.hashCode * v.id)
        } else {
          0L + v.definingInvariant.get.getClass.getCanonicalName.hashCode * v.definingInvariant.get.id
        }
      )
      .sum

    val searchHash =
      searches.map(_.name).zipWithIndex.map({ case (name, id) => name.hashCode * id }).sum

    storeHash ^ searchHash
  }

  private var myRemotelyCallableTasks: Map[Int, RemotelyCallableTask] = Map.empty

  /** @return
    *   the tasks that are remotely callable and their unique ID
    */
  def remotelyCallableTasks: Map[Int, RemotelyCallableTask] = myRemotelyCallableTasks

  /** Calls this method to declare a remotelyCallableTask.
    *
    * @param remoteTask
    *   the task instance that the system can call
    * @return
    *   the uniqueTaskClass identifier that you use to call this task remotely
    */
  def declareRemotelyCallableTask(remoteTask: RemotelyCallableTask): Int = {
    val id = myRemotelyCallableTasks.size
    myRemotelyCallableTasks += id -> remoteTask
    id
  }

  for (search <- searches) {
    search.declareRemotelyCallableTasks(this)
  }

  /** Detaches an objective from the store, so it can be sent to another store
    * @param o
    *   and objective function
    * @return
    *   a storeIndependent objective function that can be serialized and attached to another store.
    */
  def detachObjectiveFromStore(o: Objective): StoreIndependentObjective = o.detachFromStore(this)

  /** Attaches a [[StoreIndependentObjective]] objective to this store
    * @param o
    *   the detached [[StoreIndependentObjective]]
    * @return
    *   An objective that represent the same thing, within this store
    */
  def attachObjectiveToStore(o: StoreIndependentObjective): Objective = o.attachToStore(this)

  /** Detaches an objective from the store, so it can be sent to another store
    * @param variable
    *   a variable, attached to this store
    * @return
    *   an integer representing the uniqueID of the variable, which can be sent to another store,
    *   and converted back to the variable representing the same thing in the other store.
    */
  def detachVariable(variable: Variable): Int = variable.id

  /** Saves the current solution of the store in a detached solution
    * @return
    *   A StoreIndependentSolution that represent hte current solution in the store so that it can
    *   be attached to another store
    */
  def saveDetachedSolution: StoreIndependentSolution = {
    detachSolutionFromStore(store.extractSolution(Nil))
  }

  /** Detaches a [[Solution]] from the current [[Store]] so that it can be attached to another
    * [[Store]]
    * @param s
    *   a [[Solution]] attached to this store
    * @return
    *   a [[StoreIndependentSolution]] that ca nbe attached to another [[Store]]
    */
  def detachSolutionFromStore(s: Solution): StoreIndependentSolution =
    StoreIndependentSolution(
      s.valueOfDecisionVariables.map(_.makeStoreIndependent).toList,
      checkSumOnStoreAndSearch
    )

  /** Attaches a variable to the current store. It should be an [[IntVariable]] otherwise it crashes
    * @param variable
    *   the uniqueID obtained though the [[detachVariable()]] method
    * @return
    *   an intVariable representing the same thing as the variable from the store it was detached
    */
  def attachIntVarToStore(variable: Int): IntVariable =
    store.variables(variable).asInstanceOf[IntVariable]

  /** Attaches a variable to the current store. It should be a [[SetVariable]] otherwise it crashes
    * @param variable
    *   the uniqueID obtained though the [[detachVariable()]] method
    * @return
    *   a [[SetVariable]] representing the same thing as the variable from the store it was detached
    */
  def attachSetVarToStore(variable: Int): SetVariable =
    store.variables(variable).asInstanceOf[SetVariable]

  /** Attaches a variable to the current store. It should be a [[SeqVariable]] otherwise it crashes
    * @param variable
    *   the uniqueID obtained though the [[detachVariable()]] method
    * @return
    *   a [[SeqVariable]] representing the same thing as the variable from the store it was detached
    */
  def attachSeqVarToStore(variable: Int): SeqVariable =
    store.variables(variable).asInstanceOf[SeqVariable]

  /** Attaches a [[StoreIndependentSolution]] to the current store. The two stores should represen
    * the same thing otherwise it crashes
    * @param i
    *   A solution detached from another store
    * @return
    *   a solution attached to this store and representing the solution that was detached form the
    *   first store
    */
  def attachSolutionToStore(i: StoreIndependentSolution): Solution = {
    require(
      checkSumOnStoreAndSearch == i.checkSumOnStoreStructure,
      "Attaching Solution coming from a different optimization problem is nonsense"
    )
    Solution(
      valueOfDecisionVariables = i.savedValues.map {
        case StoreIndependentIntSavedValue(variableID, savedValue) =>
          store.decisionVariables.get(variableID) match {
            case Some(i: IntVariable) => IntSavedValue(i, savedValue)
            case _                    => throw new Error("not proper saved type")
          }
        case StoreIndependentSeqSavedValue(variableID, savedValue) =>
          store.decisionVariables.get(variableID) match {
            case Some(s: SeqVariable) => SeqSavedValue(s, IntSequence(savedValue))
            case _                    => throw new Error("not proper saved type")
          }

        case StoreIndependentSetSavedValue(variableID, savedValue) =>
          store.decisionVariables.get(variableID) match {
            case Some(s: SetVariable) => SetSavedValue(s, HashSet.empty ++ savedValue)
            case _                    => throw new Error("not proper saved type")
          }
        case _ => throw new Error("not proper saved type")
      },
      Nil,
      store
    )
  }

  /** Attaches a move to this store
    * @param move
    *   a [[Move]] that was detached from another [[Store]]
    * @return
    *   a [[Move]] that represent the move that was detached from the first store
    */
  def attachMoveToStore(move: StoreIndependentMove): Move =
    move.attachMoveToStore(this)

  /** Detaches a move from the current store so it can be attached to another one
    * @param move
    *   a move to detach
    * @return
    *   a detached move that can be attached to another [[Store]]
    */
  def detachMoveFromStore(move: Move): StoreIndependentMove = {
    move.detachFromStore(this)
  }
}

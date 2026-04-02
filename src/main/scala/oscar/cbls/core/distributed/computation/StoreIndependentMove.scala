package oscar.cbls.core.distributed.computation

import oscar.cbls.core.search.Move
import oscar.cbls.lib.neighborhoods.combinator.LoadSolutionMove

/** A move that is not bound to a store. It is generated from a [[oscar.cbls.core.search.Move]] and
 * can be turned back into a [[oscar.cbls.core.search.Move]]. It should be serializable.
 */
abstract class StoreIndependentMove(val objAfter: Long) {
  def attachMoveToStore(storeAdapter: SearchConnector): Move
}

case class StoreIndependentLoadSolutionMove(
                                             solution: StoreIndependentSolution,
                                             override val objAfter: Long,
                                             neighborhoodName: String
                                           ) extends StoreIndependentMove(objAfter) {
  override def attachMoveToStore(storeAdapter: SearchConnector): Move =
    LoadSolutionMove(
      storeAdapter.attachSolutionToStore(solution),
      objValueAfter = objAfter,
      neighborhoodName
    )
}

package oscar.cbls.core.distributed.search

import oscar.cbls.core.distributed.computation.SearchConnector
import oscar.cbls.core.search.{Neighborhood, NeighborhoodCombinator}

/** A handy base class for defining a distributed combinator. It will make the sub neighborhoods
  * available as remotely searchable neighborhoods so that any extension of this one class will be
  * able to run a distributed search algorithm (it is also possible to bypass this of course).
  * @param neighborhoodCombinatorName
  *   The name of the neighborhood combinator
  * @param subNeighborhoods
  *   The Neighborhood that this combinator will explore remotely; delegating the work to Workers.
  *   Those can be [[oscar.cbls.core.search.SimpleNeighborhood]] or other NeighborhoodCombinator.
  */
abstract class DistributedSimpleNeighborhoodCombinator(
  neighborhoodCombinatorName: String,
  subNeighborhoods: Array[Neighborhood]
) extends NeighborhoodCombinator(neighborhoodCombinatorName, subNeighborhoods.toList) {

  /** Every neighborhood in the constructor gets a class number in this array. The class uniquely
    * identifies the neighborhood in the distributed search framework
    */
  protected var neighborhoodToTaskClass: Array[Int] = _
  protected var searchConnector: SearchConnector    = _

  override def declareRemotelyCallableTasks(searchConnector: SearchConnector): Unit = {
    this.searchConnector = searchConnector
    neighborhoodToTaskClass = subNeighborhoods.map(neighborhood => {
      searchConnector.declareRemotelyCallableTask(RemotelySearchableNeighborhood(neighborhood))
    })
  }
}

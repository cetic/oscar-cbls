package oscar.cbls.core.distributed.computation

import oscar.cbls.Objective
import oscar.cbls.core.computation.objective.{Maximize, Minimize}

/** An objective that was detached from a [[Store]] and that cn be serialized/deserialized. It can
  * be attached to another one through the method attachToStore here below
  */
abstract class StoreIndependentObjective {

  /** attached the current detached objective to another [[Store]] using the 'attach' methods of the
    * [[SearchConnector]]
    * @param connector
    *   a connector that enables attaching variables to the targeted [[Store]]
    * @return
    *   an [[Objective]] that presents the same concept a 'this' before it was detached
    */
  def attachToStore(connector: SearchConnector): Objective
}

case class StoreIndependentMinimize(
                                     objValue: Int,
                                     mustBeZero: List[Int],
                                     underApproximatedObjValue: Option[Int]
                                   ) extends StoreIndependentObjective() {
  override def attachToStore(connector: SearchConnector): Objective = {
    Minimize(
      objValue = connector.attachIntVarToStore(objValue),
      mustBeZero = mustBeZero.map(connector.attachIntVarToStore),
      underApproximatedObjValue = underApproximatedObjValue.map(connector.attachIntVarToStore)
    )
  }
}

case class StoreIndependentMaximize(
                                     objValue: Int,
                                     mustBeZero: List[Int],
                                     overApproximatedObjValue: Option[Int]
                                   ) extends StoreIndependentObjective() {
  override def attachToStore(connector: SearchConnector): Objective = {
    Maximize(
      objValue = connector.attachIntVarToStore(objValue),
      mustBeZero = mustBeZero.map(connector.attachIntVarToStore),
      overApproximatedObjValue = overApproximatedObjValue.map(connector.attachIntVarToStore)
    )
  }
}
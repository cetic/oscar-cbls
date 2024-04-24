package oscar.cbls.core.computation.objective

import oscar.cbls.core.computation.Store

abstract class AbstractObjective(val model: Store) {

  def value: Long

  def detailedString(short: Boolean): String

}

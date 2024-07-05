// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.lib.neighborhoods

import oscar.cbls.algo.search.{HotRestart, IdenticalAggregator}
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.Exploration
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.core.search.{Move, NoMoveFound, SimpleNeighborhood}

class Assign(
  vars: Array[IntVariable],
  varsDomain: IntVariable => Range,
  name: String,
  selectVariableBehavior: LoopBehavior = LoopBehavior.first(),
  selectValueBehavior: LoopBehavior = LoopBehavior.best(),
  searchZone: Option[Iterable[Int]] = None,
  symmetryClassOfVariable: Option[Int => Int] = None,
  symmetryClassOfValue: Option[Int => Long => Long] = None,
  hotRestart: Boolean = true
) extends SimpleNeighborhood[AssignMove](name) {

  private var startIndex: Int = 0

  private var currentVar: IntVariable = _
  private var currentIndex: Int       = _
  private var newVal: Long            = _

  override protected def exploreNeighborhood(exploration: Exploration[AssignMove]): Unit = {

    // Which indices must be considered
    val iterationZone = searchZone match {
      case None     => vars.indices
      case Some(sz) => sz
    }

    // Activate hot restart if needed
    val iterationSchemeOnZone =
      if (hotRestart) HotRestart(iterationZone, startIndex)
      else iterationZone

    // Remove symmetries from considered variables
    val iterationSchemeOnSymmetryFreeZone = symmetryClassOfVariable match {
      case None => iterationSchemeOnZone
      case Some(s) =>
        IdenticalAggregator.removeIdenticalClassesLazily(
          iterationSchemeOnZone,
          (index: Int) => (s(index), vars(index).value())
        )
    }

    // How to iterate over selected variables
    val (variablesIndicesIterator, stopIndices) =
      selectVariableBehavior.toIterator(iterationSchemeOnSymmetryFreeZone)

    while (variablesIndicesIterator.hasNext) {
      currentIndex = variablesIndicesIterator.next()
      currentVar = vars(currentIndex)

      // Remove symmetries from currentVar's domain
      val domainIterationScheme = symmetryClassOfValue match {
        case None => varsDomain(currentVar)
        case Some(s) =>
          IdenticalAggregator.removeIdenticalClassesLazily(varsDomain(currentVar), s(currentIndex))
      }

      // How to iterate over domain
      val (domainIterator, stopDomain) = selectValueBehavior.toIterator(domainIterationScheme)

      while (domainIterator.hasNext) {
        val initVal = currentVar.value()
        newVal = domainIterator.next()
        currentVar := newVal
        searchProfiler().foreach(x => x.neighborSelected())

        exploration.checkNeighborWP(objValue => new AssignMove(this, newVal, objValue))
        currentVar := initVal

        if (exploration.toReturn != NoMoveFound) {
          stopIndices()
          stopDomain()
        }
      }

    }
    startIndex = currentIndex + 1

  }

  override def doMove(move: AssignMove): Unit = currentVar := move.newValue

  override def reset(): Unit = ???
}

class AssignMove(assignNeigh: Assign, val newValue: Long, objValueAfter: Long)
    extends Move(objValueAfter, assignNeigh.name) {

  override def commit(): Unit = assignNeigh.doMove(this)
}

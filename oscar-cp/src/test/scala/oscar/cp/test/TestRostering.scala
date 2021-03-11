/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.cp.test

import org.scalatest.FunSuite
import oscar.cp.testUtils.TestSuite

import oscar.cp.constraints._

import oscar.cp._
import collection.immutable.SortedSet



/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestRostering extends TestSuite  {

  test("Test Rostering2") {
    val nbPersons = 5;
    val nbSlots = 6;
    val nbActivities = 10;

    val possibleActivities = Array(Set(0, 1, 2, 3, 4, 5, 7),
                                   Set(0, 2, 3, 6, 8, 9),
                                   Set(1, 3, 4, 7, 8, 9),
                                   Set(0, 2, 4, 5, 6, 7, 8, 9),
                                   Set(0, 3, 4, 6, 7, 9));

    val demand = Array(Array(1, 0, 2, 1, 0, 0, 0, 0, 1, 0),
                       Array(2, 0, 0, 0, 0, 1, 0, 0, 0, 1),
                       Array(1, 0, 0, 1, 0, 0, 1, 0, 0, 2),
                       Array(0, 1, 0, 1, 0, 0, 0, 0, 1, 0),
                       Array(1, 0, 1, 0, 0, 1, 0, 1, 0, 1),
                       Array(0, 0, 1, 0, 0, 1, 1, 0, 1, 0));

    val cp = CPSolver()

    cp.silent=true

    val activities = Array.tabulate(nbPersons, nbSlots)((p, t) => CPIntVar(possibleActivities(p))(cp))

    val underDemand = Array.tabulate(nbSlots)(t => CPIntVar(0 to nbPersons)(cp))

    val totUnderDemand: CPIntVar = sum(underDemand)
    var best = Int.MaxValue

    // each person must do a different activity every day
    for (p <- 0 until nbPersons) {
      cp.add(allDifferent(activities(p)), Strong);
    }

    val maxCap = Array.fill(nbActivities)(nbPersons)
    for (t <- 0 until nbSlots) {
      val act_t = Array.tabulate(nbPersons)(p => activities(p)(t))
      cp.add(new oscar.cp.constraints.SoftGCCAC(act_t, 0, demand(t), maxCap, totUnderDemand))
    }

    cp.minimize(totUnderDemand) search {
      binaryFirstFail(activities.flatten.toSeq)
    } onSolution {
      best = totUnderDemand.value
    }
    cp.start
    best should be(1)

  }  


}
